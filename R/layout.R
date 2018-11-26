
## Format for laid out HTML ...
## (this may need to get more complex!)
## ... PLUS interface for accessing info
## (so that we can make it more complex without changing any other code)

layoutFields <- alist(element=,
                      x=, y=, width=, height=,
                      text=, font=, bold=, italic=, size=,
                      ## A boolean saying whether anything needs to be drawn
                      ## If backend cannot provide this, return NA
                      affectsDisplay=,
                      borderLeftWidth=, borderTopWidth=,
                      borderRightWidth=, borderBottomWidth=)

makeLayout <- function() {}
formals(makeLayout) <- layoutFields
body(makeLayout) <- quote({
    call <- match.call()
    l <- do.call(data.frame, as.list(call)[-1])
    names(l) <- names(call)[-1]
    class(l) <- c("flowedhtml", class(l))
    l
})

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
        if (layout[i, 8]) {
            face <- face + 1
        }
        if (layout[i, 9]) {
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
    } else if (is.na(layout$affectsDisplay[i]) ||
               layout$affectsDisplay[i]){
        ## An element of some sort
        ## Will almost certainly be more than one grob
        grobs <- vector("list", length(layoutFields))
        names(grobs) <- names(layoutFields)
        ## Border
        if (!is.na(layout$borderLeftWidth[i]) &&
            layout$borderLeftWidth[i] > 0) {
            grobs$borderLeftWidth <- 
                segmentsGrob(x, y, x, y + h, default.units="native",
                             gp=gpar(lwd=layout$borderLeftWidth[i]))
        } 
        if (!is.na(layout$borderTopWidth[i]) &&
            layout$borderTopWidth[i] > 0) {
            grobs$borderTopWidth <- 
                segmentsGrob(x, y + h, x + w, y + h, default.units="native",
                             gp=gpar(lwd=layout$borderTopWidth[i]))
        } 
        if (!is.na(layout$borderRightWidth[i]) &&
            layout$borderRightWidth[i] > 0) {
            grobs$borderRightWidth <- 
                segmentsGrob(x + w, y, x + w, y + h, default.units="native",
                             gp=gpar(lwd=layout$borderRightWidth[i]))
        } 
        if (!is.na(layout$borderBottomWidth[i]) &&
            layout$borderBottomWidth[i] > 0) {
            grobs$borderBottomWidth <- 
                segmentsGrob(x, y, x + w, y, default.units="native",
                             gp=gpar(lwd=layout$borderBottomWidth[i]))
        }
        ## Faint bbox outline
        ## rectGrob(x, y, w, h, default.units="native",
        ##          just=c("left", "bottom"), gp=gpar(lwd=.1, fill=NA))
        gTree(children=do.call(gList, grobs[!sapply(grobs, is.null)]))
    }
}

layoutGrobs <- function(x) {
    do.call("gList", lapply(1:nrow(x), boxGrob, x))
}

