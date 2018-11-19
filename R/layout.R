
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

## Generate grobs from laid out HTML
boxGrob <- function(i, layout) {
    ## Y measure down from top in web browser
    totalHeight <- convertHeight(unit(1, "npc"), "in", valueOnly=TRUE)
    x <- layout[i, 2]/dpi
    y <- totalHeight - layout[i, 3]/dpi
    w <- layout[i, 4]/dpi
    h <- -layout[i, 5]/dpi
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
                 unit(x, "in"),
                 unit(y + h, "in") + grobDescent(fontgrob),
                 just=c("left", "bottom"),
                 gp=gpar(fontfamily=layout[i, 7], fontface=face,
                         fontsize=layout[i, 10]))
    } else {
        rectGrob(x, y, w, h, default.units="in", just=c("left", "bottom"),
                 gp=gpar(lwd=.1, fill=NA))
    }
}

## Engine may specify its own grob function
layoutGrobs <- function(x) {
    do.call("gList", lapply(1:nrow(x), boxGrob, x))
}

