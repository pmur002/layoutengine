\name{cssFontFamily}
\alias{cssFontFamily}
\title{
  Map R Fonts to CSS Fonts
}
\description{
  This function maps an R font family to a CSS font family.  This is
  useful when generating HTML content.
}
\usage{
cssFontFamily(fonts, device=currentDevice()) 
}
\arguments{
  \item{fonts}{
    A character vector containing R font family names.
  }
  \item{device}{
    The name of the device that will be used to render the layout.
  }
}
\value{
  The CSS font family names.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.html}}
}
\examples{
cssFontFamily("sans")
}
\keyword{ dplot }
