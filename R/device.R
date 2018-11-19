
currentDevice <- function() {
    dev <- dev.cur()
    ## If no devices, open one
    if (dev == 1) {
        dev.new()
        dev <- dev.cur()
    }
    names(dev)
}
