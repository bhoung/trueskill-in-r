\name{GaussianOperators}
\title{Gaussians Operators}
\alias{Multiply}
\alias{Divide}
\description{
  multiply or divide Gaussians
  equiavelent to the more convenient inline operator "*" or "/".
  
  x * y
  
  x / y
  
  z <- x * y
  
  z2 <- x / y

}
\section{trueskill-package:}{
Refer to \code{\link{trueskill-package}} for links to related classes, functions and data.
}
\usage{
  Multiply(x, y)
  Divide(x, y)
}
\arguments{
  \item{x}{a gaussian object x}
  \item{y}{a gaussian object y}
}
\examples{
x <- Gaussian(25, 8)
y <- Gaussian(20, 6)
}
