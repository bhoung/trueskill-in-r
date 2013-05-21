\name{GaussianOperators}
\title{Gaussians Operators}
\alias{Multiply}
\alias{Divide}
\usage{
  Multiply(x, y)
  Divide(x, y)
}
\arguments{
  \item{x}{a gaussian object x}
  \item{y}{a gaussian object y}
}
\description{
  multiply or divide Gaussians
  equiavelent to the more convenient inline operator "*" or "/".
  
  x * y
  
  x / y
  
  z <- x * y
  
  z2 <- x / y

}
\examples{
x <- Gaussian(25, 8)
y <- Gaussian(20, 6)
}
