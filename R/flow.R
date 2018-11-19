
## Lay out HTML

flow <- function(html, width, height, fonts, device, engine, ...) {
    UseMethod("flow")
}

flowDoc <- function(html, width, height, fonts, device, engine, ...) {
    ## Add font info to document 
    head <- xml_add_child(html, "head", .where=0)
    xml_add_child(head,
                  "style",
                  type="text/css",
                  fontCSS(fonts, device),
                  paste0('\nbody { font-family: "',
                         firstFont(fonts, device), '" }'))
    engine$layout(html, width, height, fonts, device)
}

flow.htmlDocument <- function(html, width, height, fonts, device, engine, ...) {
    flowDoc(html, width, height, fonts, device, engine, ...)
}

flow.htmlElement <- function(html, width, height, fonts, device, engine, ...) {
    layout <- flowDoc(html, width, height, fonts, device, engine, ...)
    ## Having flowed HTML document, extract just the element of interest
    stripLayout(layout)
}
