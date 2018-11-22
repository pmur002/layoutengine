
## Format for laid out HTML ...
## (this may need to get more complex!)
## ... PLUS interface for accessing info
## (so that we can make it more complex without changing any other code)

makeLayout <- function(element,
                       x, y, width, height,
                       text, font, bold, italic, size) {
    l <- data.frame(element,
                    x, y, width, height,
                    text, font, bold, italic, size,
                    stringsAsFactors=FALSE)
    class(l) <- c("flowedhtml", class(l))
    l
}

length.flowedhtml <- function(x) {
    nrow(x)
}

stripLayout <- function(x) {
    ## Remove the <html> and <body> info
    x[!grepl("^(html|body)$", x$element, ignore.case=TRUE), ]
}

## CSS standard says 1px = 1/96in !?
dpi <- 96

layoutWidth <- function(x) {
    (max(x$x + x$width) - min(x$x))/dpi
}

layoutHeight <- function(x) {
    (max(x$y + x$height) - min(x$y))/dpi
}

layoutXScale <- function(x) {
    range(x$x, x$x + x$width)
}

layoutYScale <- function(x) {
    rev(range(x$y, x$y + x$height))
}

## Generate grobs from laid out HTML
boxGrob <- function(i, layout) {
    ## Y measure down from top in web browser
    totalHeight <- convertHeight(unit(1, "npc"), "native", valueOnly=TRUE)
    x <- layout[i, 2]
    y <- layout[i, 3]
    w <- layout[i, 4]
    h <- layout[i, 5]
    if (layout[i, 1] == "TEXT") {
        face <- 1
        if (layout[i, 8] == "true") {
            face <- face + 1
        }
        if (layout[i, 9] == "true") {
            face <- face + 2
        }
        fontgrob <- textGrob(paste(c(letters, LETTERS), collapse=""),
                             gp=gpar(fontfamily=layout[i, 7], fontface=face,
                                     fontsize=layout[i, 10]))
        textGrob(layout[i, 6],
                 unit(x, "native"),
                 unit(y + h, "native") + grobDescent(fontgrob),
                 just=c("left", "bottom"),
                 gp=gpar(fontfamily=layout[i, 7], fontface=face,
                         fontsize=layout[i, 10]))
    } else {
        rectGrob(x, y, w, h, default.units="native", just=c("left", "bottom"),
                 gp=gpar(lwd=.1, fill=NA))
    }
}

layoutGrobs <- function(x) {
    do.call("gList", lapply(1:nrow(x), boxGrob, x))
}

