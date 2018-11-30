
## 'x' should be an htmlDocument
copyAssets <- function(x, dir) {
    assets <- attr(x, "assets")
    if (!is.null(assets)) {
        file.copy(assets, dir)
    }
}
