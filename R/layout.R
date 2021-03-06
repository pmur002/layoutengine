
## Format for laid out HTML ...
## (this may need to get more complex!)
## ... PLUS interface for accessing info
## (so that we can make it more complex without changing any other code)

layoutFields <- alist(type=, name=,
                      ## Box
                      x=, y=, width=, height=,
                      ## Text
                      baseline=, text=, family=, bold=, italic=, size=, color=,
                      direction=,
                      ## Borders
                      backgroundColor=,
                      borderLeftWidth=, borderTopWidth=,
                      borderRightWidth=, borderBottomWidth=,
                      borderLeftStyle=, borderTopStyle=,
                      borderRightStyle=, borderBottomStyle=,
                      borderLeftColor=, borderTopColor=,
                      borderRightColor=, borderBottomColor=,
                      ## Lists
                      listStyleType=, listStylePosition=)

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

## Y measure down from top in web browser
layoutYrange <- function(x) {
    c(min(x$y), max(x$y + x$height))
}

layoutYScale <- function(x) {
    c(0, diff(layoutYrange(x)))
}

col <- function(x) {
    if (is.na(x) || nchar(x) == 0) {
        "black"
    } else {
        x
    }
}

## Default col is "black"
transparentCol <- function(x) {
    !(is.na(x) || nchar(x) == 0) &&
        (x == "transparent" || col2rgb(x, alpha=TRUE)[4] == 0)
}

## Default fill is "transparent"
transparentFill <- function(x) {
    is.na(x) ||
        nchar(x) == 0 ||
        x == "transparent" ||
        col2rgb(x, alpha=TRUE)[4] == 0
}

## Default border style is "solid" ?
lty <- function(x) {
    
    if (is.na(x))
        return("solid")

    lty <- switch(x,
                  ## Supported
                  none=,
                  hidden=,
                  dotted=,
                  dashed=,
                  solid=x,
                  ## Unsupported
                  double=,
                  groove=,
                  ridge=,
                  inset=,
                  outset="solid")
    if (lty != x)
        warning('Unsupported border style converted to "solid"')
    lty
}

drawBorder <- function(border, i, layout) {
    !is.na(layout[[paste0("border", border, "Width")]][i]) &&
        (layout[[paste0("border", border, "Width")]][i] > 0) &&
        (!layout[[paste0("border", border, "Style")]][i] %in%
         c("none", "hidden")) &&
        !transparentCol(layout[[paste0("border", border, "Color")]][i])    
}

## List item bullets
supportedBullets <- c("disc", "circle", "square")

bulletGrob <- function(x, y, w, h, type, position, direction, size, colour) {
    bulletY <- y + h/2
    if (direction == "ltr") {
        bulletX <- x
        if (position == "inside") {
            dir <- 1
            offset <- unit(0, "lines")
            just <- "left"
        } else {
            dir <- -1
            offset <- unit(-.5*size, "pt")
            just <- "right"
        }
    } else {
        bulletX <- x + w
        if (position == "inside") {
            dir <- -1
            offset <- unit(0, "lines")
            just <- "right"
        } else {
            dir <- 1
            offset <- unit(.5*size, "pt")
            just <- "left"
        }
    }
    if (type == "disc") {
        circleGrob(unit(bulletX, "native") + offset + dir*unit(.2*size, "pt"),
                   unit(bulletY, "native"), 
                   r=unit(.2*size, "pt"),
                   gp=gpar(col=colour, fill=colour))
    } else if (type == "circle") {
        circleGrob(unit(bulletX, "native") + offset + dir*unit(.2*size, "pt"),
                   unit(bulletY, "native"), 
                   r=unit(.2*size, "pt"),
                   gp=gpar(col=colour, fill=NA))
    } else if (type == "square") {
        rectGrob(unit(bulletX, "native") + offset,
                 unit(bulletY, "native"),
                 width=unit(.4*size, "pt"), height=unit(.4*size, "pt"),
                 just=just,
                 gp=gpar(col=colour, fill=colour))
    }        
}

## Only take notice of specific elements
supportedElements <- c("DIV", "P", "SPAN",
                       "TABLE", "TBODY", "TR", "TH", "TD",
                       "PRE", "CODE",
                       "H1", "H2", "H3", "H4", "H5", "H6",
                       "LI")
supportedElement <- function(x) {
    toupper(x) %in% supportedElements
}

boxGrob <- function(i, layout, yrange) {
    ## Y measure down from top in web browser
    x <- layout$x[i]
    y <- yrange[2] - (layout$height[i] + layout$y[i])
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
        ## Remove leading or trailing white space
        tg <- textGrob(gsub("^ +| +$", "", layout$text[i]),
                       unit(x, "native"),
                       unit(y + layout$baseline[i], "native"),
                       just=c("left", "bottom"),
                       gp=gpar(fontfamily=layout$family[i], fontface=face,
                               fontsize=layout$size[i],
                               col=col(layout$color[i])),
                       name=layout$name[i])
        if (getOption("layoutEngine.debug")) {
            gTree(children=gList(rectGrob(x, y, w, h, default.units="native",
                                          just=c("left", "bottom"),
                                          gp=gpar(lwd=.1)),
                                 tg))
        } else {
            tg
        }
    } else if (supportedElement(layout$type[i])) {
        ## An element of some sort
        ## Will almost certainly be more than one grob
        grobs <- vector("list", length(layoutFields))
        names(grobs) <- names(layoutFields)
        ## Background
        if (!transparentFill(layout$backgroundColor[i])) {
            grobs$backgroundColor <-
                rectGrob(x, y, w, h, default.units="native",
                         just=c("left", "bottom"),
                         gp=gpar(col=NA, fill=col(layout$backgroundColor[i])),
                         name="background")
        }
        ## Border
        if (drawBorder("Left", i, layout)) {
            lwd <- layout$borderLeftWidth[i]
            grobs$borderLeftWidth <- 
                segmentsGrob(x + lwd/2, y, x + lwd/2, y + h,
                             default.units="native",
                             gp=gpar(lineend="butt",
                                     lwd=lwd,
                                     col=col(layout$borderLeftColor[i]),
                                     lty=lty(layout$borderLeftStyle[i])),
                             name="border.left")
        } 
        if (drawBorder("Top", i, layout)) {
            lwd <- layout$borderTopWidth[i]
            grobs$borderTopWidth <- 
                segmentsGrob(x, y + h - lwd/2, x + w, y + h - lwd/2,
                             default.units="native",
                             gp=gpar(lineend="butt",
                                     lwd=lwd,
                                     col=col(layout$borderTopColor[i]),
                                     lty=lty(layout$borderTopStyle[i])),
                             name="border.top")
        } 
        if (drawBorder("Right", i, layout)) {
            lwd <- layout$borderRightWidth[i]
            grobs$borderRightWidth <- 
                segmentsGrob(x + w - lwd/2, y, x + w - lwd/2, y + h,
                             default.units="native",
                             gp=gpar(lineend="butt",
                                     lwd=lwd,
                                     col=col(layout$borderRightColor[i]),
                                     lty=lty(layout$borderRightStyle[i])),
                             name="border.right")
        } 
        if (drawBorder("Bottom", i, layout)) {
            lwd <- layout$borderBottomWidth[i]
            grobs$borderBottomWidth <- 
                segmentsGrob(x, y + lwd/2, x + w, y + lwd/2,
                             default.units="native",
                             gp=gpar(lineend="butt",
                                     lwd=lwd,
                                     col=col(layout$borderBottomColor[i]),
                                     lty=lty(layout$borderBottomStyle[i])),
                             name="border.bottom")
        }
        ## List item "bullets"
        if (toupper(layout$type[i]) == "LI") {
            if (layout$listStyleType[i] %in% supportedBullets) {
                grobs$listStyleType <-
                    bulletGrob(x, y, w, h, layout$listStyleType[i],
                               layout$listStylePosition[i],
                               layout$direction[i],
                               layout$size[i],
                               layout$color[i])
            } else if (layout$lsitStyleType[i] != "none") {
                warning("Unsupported list-style-type (bullet not drawn)")
            }
        }
        gTree(children=do.call(gList, grobs[!sapply(grobs, is.null)]),
                 name=layout$name[i])
    }
}

layoutGrobs <- function(x) {
    yrange <- layoutYrange(x)
    do.call("gList", lapply(1:nrow(x), boxGrob, x, yrange))
}

boxViewport <- function(i, layout, yrange) {
    ## Y measure down from top in web browser
    x <- layout$x[i]
    y <- yrange[2] - (layout$height[i] + layout$y[i])
    w <- layout$width[i]
    h <- layout$height[i]
    if (grepl("^text", layout$type[i], ignore.case=TRUE)) {
        ## No viewports from text nodes
        NULL
    } else if (layout$type[i] %in% supportedElements) {
        ## An element of some sort
        viewport(x, y, w, h, default.units="native",
                 just=c("left", "bottom"),
                 name=paste0(layout$name[i], ".vp"))
    }
}

layoutViewports <- function(x) {
    yrange <- layoutYrange(x)
    viewports <- lapply(1:nrow(x), boxViewport, x, yrange)
    vplist <- viewports[!sapply(viewports, is.null)]
    if (length(vplist)) {
        do.call("vpList", vplist)
    } else {
        NULL
    }
}

