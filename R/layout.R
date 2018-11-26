
## Format for laid out HTML ...
## (this may need to get more complex!)
## ... PLUS interface for accessing info
## (so that we can make it more complex without changing any other code)

layoutFields <- alist(type=, name=, 
                      x=, y=, width=, height=,
                      text=, family=, bold=, italic=, size=,
                      ## A boolean saying whether anything needs to be drawn
                      ## If backend cannot provide this, return NA
                      affectsDisplay=,
                      borderLeftWidth=, borderTopWidth=,
                      borderRightWidth=, borderBottomWidth=)

makeLayout <- function() {}
formals(makeLayout) <- layoutFields
body(makeLayout) <- quote({
    call <- match.call()
    l <- do.call(data.frame,
                 c(as.list(call)[-1], list(stringsAsFactors=FALSE)))
    names(l) <- names(call)[-1]
    class(l) <- c("flowedhtml", class(l))
    l
})

length.flowedhtml <- function(x) {
    nrow(x)
}

stripLayout <- function(x) {
    ## Remove the <html> and <body> info
    x[!grepl("^(html|body)$", x$type, ignore.case=TRUE), ]
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

boxGrob <- function(i, layout) {
    ## Y measure down from top in web browser
    totalHeight <- convertHeight(unit(1, "npc"), "native", valueOnly=TRUE)
    x <- layout$x[i]
    y <- layout$y[i]
    w <- layout$width[i]
    h <- layout$height[i]
    if (grepl("^text", layout$type[i], ignore.case=TRUE)) {
        face <- 1
        if (layout$bold[i]) {
            face <- face + 1
        }
        if (layout$italic[i]) {
            face <- face + 2
        }
        fontgrob <- textGrob(paste(c(letters, LETTERS), collapse=""),
                             gp=gpar(fontfamily=layout$family[i], fontface=face,
                                     fontsize=layout$size[i]))
        textGrob(layout$text[i],
                 unit(x, "native"),
                 unit(y + h, "native") + grobDescent(fontgrob),
                 just=c("left", "bottom"),
                 gp=gpar(fontfamily=layout$family[i], fontface=face,
                         fontsize=layout$size[i]),
                 name=layout$name[i])
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
                             gp=gpar(lwd=layout$borderLeftWidth[i]),
                             name="border.left")
        } 
        if (!is.na(layout$borderTopWidth[i]) &&
            layout$borderTopWidth[i] > 0) {
            grobs$borderTopWidth <- 
                segmentsGrob(x, y + h, x + w, y + h, default.units="native",
                             gp=gpar(lwd=layout$borderTopWidth[i]),
                             name="border.top")
        } 
        if (!is.na(layout$borderRightWidth[i]) &&
            layout$borderRightWidth[i] > 0) {
            grobs$borderRightWidth <- 
                segmentsGrob(x + w, y, x + w, y + h, default.units="native",
                             gp=gpar(lwd=layout$borderRightWidth[i]),
                             name="border.right")
        } 
        if (!is.na(layout$borderBottomWidth[i]) &&
            layout$borderBottomWidth[i] > 0) {
            grobs$borderBottomWidth <- 
                segmentsGrob(x, y, x + w, y, default.units="native",
                             gp=gpar(lwd=layout$borderBottomWidth[i]),
                             name="border.bottom")
        }
        gTree(children=do.call(gList, grobs[!sapply(grobs, is.null)]),
                 name=layout$name[i])
    }
}

layoutGrobs <- function(x) {
    do.call("gList", lapply(1:nrow(x), boxGrob, x))
}

