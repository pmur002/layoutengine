\name{htmlDocument}
\alias{htmlDocument}
\alias{htmlDocument.character}
\alias{htmlDocument.xml_document}
\alias{htmlElement}
\alias{htmlElement.character}
\alias{htmlElement.xml_node}
\title{
  Describe HTML Content
}
\description{
  These functions allow us to describe HTML content that we want to
  render.  \code{htmlDocument} is useful when we have a complete
  HTML document to render
  and \code{htmlElement} is useful when we just have
  an HTML element to render.
}
\usage{
htmlDocument(x, \dots)
\method{htmlDocument}{character}(x, assets=NULL, \dots)
\method{htmlDocument}{xml_document}(x, assets=NULL, \dots)
htmlElement(x, \dots)
\method{htmlElement}{character}(x, css="", assets=NULL, \dots)
\method{htmlElement}{xml_node}(x, css="", assets=NULL, \dots)
}
\arguments{
  \item{x}{
    Either a character vector or an XML object (from the \pkg{xml2}
    package) describing HTML content.  
  }
  \item{css}{
    A character vector describing CSS styling for the HTML content.
  }
  \item{assets}{
    Zero or more paths to external assets (e.g., image files) for
    the HTML content.
  }
  \item{\dots}{
    Arguments used by methods.
  }
}
\value{
  An \code{"htmlDocument"} object or an \code{"htmlElement"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.html}}
}
\examples{
htmlElement("<p>test</p>")
}
\keyword{ dplot }
