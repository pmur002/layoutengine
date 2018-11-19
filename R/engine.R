
## Interface for a layoutEngine backend
## 'layout' is a function

makeEngine <- function(layout) {
    engine <- list(layout=layout)
    class(engine) <- "layoutengine"
    engine
}

## Just draws the HTML itself (!)
nullLayout <- function(html, width, height, fonts, device) {
    layout("TEXT", 0, 0, width*dpi, height*dpi,
           as.character(html), fonts[1], "false", "false", 10)
}
nullEngine <- makeEngine(nullLayout)
