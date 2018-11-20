
currentDevice <- function() {
    dev <- dev.cur()
    ## If no devices, open one
    if (dev == 1) {
        dev.new()
        dev <- dev.cur()
    }
    names(dev)
}

cairoDevice <- function(device) {
    ## X11cairo, cairo_pdf, cairo_ps
    grepl("cairo", device) ||  
        ## NOTE that png(type="Xlib") has name "PNG" (all caps), etc
        device %in% c("svg", "png", "jpeg", "tiff", "bmp")
}
