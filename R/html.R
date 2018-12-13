
## Interface for describing an HTML document to 'layoutEngine'

buildDoc <- function(html, css="") {
    doc <- read_html(html)
    if (nchar(css) > 0) {
        ## add CSS to document (adding <head> if necessary)
        head <- xml_find_first(doc, "head")
        if (is.na(head)) 
            head <- xml_add_child(doc, "head", .where=0)
        xml_add_child(head,
                      "style",
                      type="text/css",
                      css)
    }
    doc
}

## A complete HTML document (in some format)
htmlDocument <- function(x, ...) {
    UseMethod("htmlDocument")
}

## Assume character value is HTML
htmlDocument.character <- function(x, 
                                   css="",
                                   assets=NULL,
                                   ...) {
    doc <- buildDoc(paste(x, collapse=""), css)
    obj <- list(doc=doc, assets=assets)
    class(obj) <- c("htmlDocument")
    obj
}

## If already an xml2::xml_document, just check it is HTML
htmlDocument.xml_document <- function(x, 
                                      css="",
                                      assets=NULL,
                                      ...) {
    doc <- buildDoc(as.character(x), css)
    obj <- list(doc=doc, assets=assets)
    class(obj) <- c("htmlDocument")
    obj
}

## A standalone element
## (which we wrap within an HTML document,
##  BUT keep track of the element of interest)
htmlElement <- function(x, ...) {
    UseMethod("htmlElement")
}

## Assume character value is HTML element
htmlElement.character <- function(x,
                                  css="",
                                  assets=NULL,
                                  ...) {
    ## Will auto-complete HTML doc (with element as only content within body)
    doc <- buildDoc(paste(x, collapse=""), css)
    obj <- list(doc=doc, assets=assets)
    class(obj) <- c("htmlElement", "htmlDocument")
    obj
}

## Valid XML, just check ok HTML
htmlElement.xml_node <- function(x,
                                 css="",
                                 assets=NULL,
                                 ...) {
    doc <- buildDoc(as.character(x), css)
    obj <- list(doc=doc, assets=assets)
    class(obj) <- c("htmlElement", "htmlDocument")
    obj
}

