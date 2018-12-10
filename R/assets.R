
## 'x' should be an htmlDocument
copyAssets <- function(html, dir) {
    assets <- html$assets
    if (!is.null(assets)) {
        file.copy(assets, dir)
    }
}
