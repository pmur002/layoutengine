
## Lay out HTML

flow <- function(html, ...) {
    UseMethod("flow")
}

flowDoc <- function(html, width, height, fonts, device, engine) {
    ## Add font info to document (adding <head> if necessary)
    head <- xml_find_first(html$doc, "head")
    if (is.na(head)) 
        head <- xml_add_child(html$doc, "head", .where=0)
    xml_add_child(head,
                  "style",
                  type="text/css",
                  fontCSS(fonts, device, engine$cssTransform),
                  paste0('\nbody { font-family: "',
                         firstFont(fonts, device), '" }'))
    if (is.null(width))
        width <- unit(1, "npc")
    if (is.null(height))
        height <- unit(1, "npc")
    engine$layout(html,
                  convertWidth(width, "in", valueOnly=TRUE),
                  convertHeight(height, "in", valueOnly=TRUE),
                  fonts, device)
}

## If not already an HTML element or document, convert to such
flow.default <- function(html, ..., css="", assets=NULL) {
    flow(htmlElement(html, css, assets), ...)
}

flow.htmlDocument <- function(html, width=NULL, height=NULL,
                              fonts="sans",
                              device=currentDevice(),
                              engine=getOption("layoutEngine.backend"),
                              ...) {
    flowDoc(html, width, height, fonts, device, engine)
}

flow.htmlElement <- function(html, width=NULL, height=NULL,
                             fonts="sans",
                             device=currentDevice(),
                             engine=getOption("layoutEngine.backend"),
                             ...) {
    layout <- flowDoc(html, width, height, fonts, device, engine)
    ## Having flowed HTML document, extract just the element of interest
    stripLayout(layout)
}
