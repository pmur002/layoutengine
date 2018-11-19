
## 'grid' style interface

## Accept a variety of input
htmlGrob <- function(html, ...) {
    UseMethod("htmlGrob")
}

## Assume that input is some expression of an HTML element
## and pass to HTMLElement() can handle
htmlGrob.default <- function(html, ...) {
    htmlGrob(htmlElement(html), ...)
}

## Input that has already been through htmlElement() or htmlDocument(),
## but is not laid out
makeContent.htmlgrob <- function(x) {
    w <- convertWidth(x$width, "in", valueOnly=TRUE)
    h <- convertHeight(x$height, "in", valueOnly=TRUE)
    layout <- flow(x$html, w, h, x$fonts, x$device, x$engine)
    ## Do the same thing as htmlGrob.flowedHTML
    setChildren(x, layoutGrobs(layout))
}

htmlGrob.htmlDocument <- function(html, 
                                  x=0.5, y=0.5, width=1, height=1,
                                  default.units="npc",
                                  just="centre",
                                  fonts="sans",
                                  device=currentDevice(),
                                  engine=getOption("layoutEngine.backend"),
                                  gp=gpar(), name=NULL, ...) {
    vp <- viewport(x, y, width, height, default.units, just)
    if (!is.unit(width)) {
        width <- unit(width, default.units)
    }
    if (!is.unit(height)) {
        height <- unit(height, default.units)
    }
    ## Just record all the info
    ## Flow when render (via makeContent method)
    gTree(html=html,
          width=width, height=height,
          fonts=fonts, device=device, engine=engine,
          gp=gp, name=name, vp=vp, cl="htmlgrob")
}

## Laid out HTML
htmlGrob.flowedhtml <- function(html,
                                x=0.5, y=0.5, width=1, height=1,
                                default.units="npc",
                                just="centre",
                                gp=gpar(), name=NULL, ...) {
    ## Grobs representing the laid out HTML
    ## Can build this as fixed gTree (layout has already happened so is fixed)
    vp <- viewport(x, y, width, height, default.units, just)
    gTree(children=layoutGrobs(html),
          gp=gp, name=name, vp=vp, cl="flowedhtmlgrob")
}
    
grid.html <- function(...) {
    grid.draw(htmlGrob(...))
}
