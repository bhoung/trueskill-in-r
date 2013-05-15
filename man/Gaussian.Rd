\name{Gaussian}
\alias{Gaussian}
\title{Gaussian Class with args (mu, sigma) or (pi, tau)}
\usage{
  Gaussian(mu, sigma)
  Gaussian(pi, tau)
}
\arguments{
  \item{mu}{mean skill}

  \item{sigma}{std dev of skill}

  \item{pi}{precision ( 1 / sigma^2)}

  \item{tau}{precision adjusted mean (mu / sigma^2)}
}
\description{
	
  Reference Class to create objects that represent normal distributions.
  
  Gaussian$new(25, 8) or Gaussian(25, 8) is equivalent
  
  Defaults to mu = 0, sigma = Inf or tau = 0 and pi = 0.
  
  Also has accompanying functions Multiply(g1, g2) and Divide(g1, g2).
  
  The functions are not methods as a new copy of the Gaussian is produced (see example below).
  
}
\section{Methods}{
  \describe{
    \item{\code{MuSigma():}}{ returns list of c(mu, sigma) }
    \item{\code{mu()}}{ returns mu }
    \item{\code{sigma()}}{ returns sigma }
  }
} 
\examples{
	
  g0 <- Gaussian(pi = 0.05, tau = 0.13)	
	
  g1 <- Gaussian(mu = 25, sigma = 8)
  g2 <- Gaussian(30, 6)
  g3 <- Multiply(g1, g2)
  
}
