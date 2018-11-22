
## Interface for describing an HTML document to 'layoutEngine'

## A complete HTML document (in some format)
htmlDocument <- function(x, ...) {
    UseMethod("htmlDocument")
}

## Assume character value is HTML
htmlDocument.character <- function(x, ...) {
    doc <- read_html(x)
    class(doc) <- c("htmlDocument", class(doc))
    doc
}

## If already an xml2::xml_document, just check it is HTML
htmlDocument.xml_document <- function(x, ...) {
    doc <- read_html(as.character(x))
    class(doc) <- c("htmlDocument", class(doc))
    doc
}

## A standalone element
## (which we wrap within an HTML document,
##  BUT keep track of the element of interest)
htmlElement <- function(x, ...) {
    UseMethod("htmlElement")
}

## Assume character value is HTML element
htmlElement.character <- function(x, ...) {
    ## Will auto-complete HTML doc (with element as only content within body)
    doc <- read_html(x)
    class(doc) <- c("htmlElement", "htmlDocument", class(doc))
    doc
}

## Valid XML, just check ok HTML
htmlElement.xml_node <- function(x, ...) {
    doc <- read_html(tags$html(tags$body(HTML(as.character(x)))))
    class(doc) <- c("htmlElement", "htmlDocument", class(doc))
    doc
}

## Extensions for likely packages

## An 'xtable' table
htmlElement.xtable <- function(x, ...) {
    htmlElement(print(x, "html", print.results=FALSE), ...)
}

