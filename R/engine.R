
## Interface for a layoutEngine backend
## 'layout' is a function
##   (for laying out HTML)
## 'cssTransform' is a list of functions
##   (for converting/transforming CSS properties)

makeEngine <- function(layout, cssTransform=list()) {
    engine <- list(layout=layout, cssTransform=cssTransform)
    class(engine) <- "layoutengine"
    engine
}

## Just returns the HTML itself within the requested box size
nullLayout <- function(html, width, height, fonts, device) {
    layoutArgs <- list("TEXT", "text.1", 0, 0, width*dpi, height*dpi,
                       as.character(html), fonts[1], FALSE, FALSE, 10, "black",
                       "transparent",
                       NA, NA, NA, NA,
                       NA, NA, NA, NA,
                       NA, NA, NA, NA)
    do.call(makeLayout, layoutArgs)
}

nullEngine <- makeEngine(nullLayout)
