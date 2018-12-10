
## 'grid' style interface

## Accept a variety of input
htmlGrob <- function(html, ...) {
    UseMethod("htmlGrob")
}

## Assume that input is some expression of an HTML element
## and pass to HTMLElement() can handle
htmlGrob.default <- function(html, ..., css="", assets=NULL) {
    htmlGrob(htmlElement(html, css, assets), ...)
}

htmlViewport <- function(html, x, y, just) {
    viewport(x, y, layoutWidth(html), layoutHeight(html),
             default.units="in", just=just,
             xscale=layoutXScale(html), yscale=layoutYScale(html))
}

## Input that has already been through htmlElement() or htmlDocument(),
## but is not laid out
widthDetails.htmlgrob <- function(x) {
    layout <- flow(x$html, 
                   x$width, x$height, x$fonts, x$device, x$engine)
    unit(layoutWidth(layout), "in")
}
heightDetails.htmlgrob <- function(x) {
    layout <- flow(x$html, 
                   x$width, x$height, x$fonts, x$device, x$engine)
    unit(layoutHeight(layout), "in")
}

makeContext.htmlgrob <- function(x) {
    layout <- flow(x$html, 
                   x$width, x$height, x$fonts, x$device, x$engine)
    x$vp <- htmlViewport(layout, x$x, x$y, x$just)
    if (x$viewports) {
        x$childrenvp <- layoutViewports(layout)
    }
    ## Preserve layout so we do not have to recalculate it in
    ## makeContent() method
    x$layout <- layout
    x
}

makeContent.htmlgrob <- function(x) {
    setChildren(x, layoutGrobs(x$layout))
}

htmlGrob.htmlDocument <- function(html, 
                                  x=0.5, y=0.5,
                                  width=NULL, height=NULL,
                                  default.units="npc",
                                  just="centre",
                                  fonts="sans",
                                  device=currentDevice(),
                                  engine=getOption("layoutEngine.backend"),
                                  viewports=FALSE,
                                  gp=gpar(), name=NULL, ...) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.null(width) && !is.unit(width)) {
        width <- unit(width, default.units)
    }
    if (!is.null(height) && !is.unit(height)) {
        height <- unit(height, default.units)
    }
    ## Just record all the info
    ## Flow when render (via makeContent method)
    gTree(html=html, x=x, y=y, just=just,
          width=width, height=height,
          fonts=fonts, device=device, engine=engine,
          viewports=viewports,
          gp=gp, name=name, cl="htmlgrob")
}

## Laid out HTML (already has a size)
widthDetails.flowedhtmlgrob <- function(x) {
    unit(layoutWidth(x$layout), "in")
}

heightDetails.flowedhtmlgrob <- function(x) {
    unit(layoutHeight(x$layout), "in")
}

htmlGrob.flowedhtml <- function(html, 
                                x=0.5, y=0.5, 
                                default.units="npc",
                                just="centre",
                                viewports=FALSE,
                                gp=gpar(), name=NULL, ...) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    ## Grobs representing the laid out HTML
    ## Can build this as fixed gTree (layout has already happened so is fixed)
    vp <- htmlViewport(html, x, y, just)
    if (viewports) {
        childrenvp <- layoutViewports(html)
    } else {
        childrenvp <- NULL
    }
    gTree(layout=html,
          children=layoutGrobs(html),
          childrenvp=childrenvp,
          gp=gp, name=name, vp=vp, cl="flowedhtmlgrob")
}
    
grid.html <- function(...) {
    grid.draw(htmlGrob(...))
}
