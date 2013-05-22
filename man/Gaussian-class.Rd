\name{Gaussian-class}
\alias{Gaussian-class}
\alias{Gaussian}
\alias{*,Gaussian,Gaussian-method}
\alias{/,Gaussian,Gaussian-method}
\title{Gaussian Class with args (mu, sigma) or (pi, tau)}
\description{
Reference Class to create objects that represent normal distributions, which is how players' skills are represented.  
Gaussian takes arguments (mu, sigma) or (pi, tau), which default to (0, Inf) or (0, 0), respectively.
% 
Note: for consistency reasons and not having to update two sets of values, the class only stores pi and tau.
Therefore, g1$mu <- 25 and g1$sigma <- 8 does not work as expected, though g1$pi <- 0.04 and g1$tau <- 0.13 does.
} 
\section{Usage:}{
  Gaussian(mu, sigma)
  
  Gaussian(pi, tau)
}

\section{Arguments:}{ 
  \describe{  
    \item{mu}{is the mean skill \deqn{\mu}} 
    \item{sigma}{is the std dev of skill \deqn{\sigma}}
    \item{pi}{is the precision \deqn{( 1 / \sigma^2)}}
    \item{tau}{is the precision adjusted mean \deqn{(\mu / \sigma^2)}}  
  }
}


\section{Methods:}{
  \describe{
    \item{*}{\code{signature(e1 = Gaussian, e2 = Gaussian)}:...}
    \item{/}{\code{signature(e1 = Gaussian, e2 = Gaussian)}:...}
    \item{\code{MuSigma():}}{ returns a list of c(mu, sigma) }
    \item{\code{mu():}}{ returns mu }
    \item{\code{sigma():}}{ returns sigma } 
  }
}
\seealso{
		
\code{\link{Divide}} and \code{\link{Multiply}}		
These functions are not methods as a new copy of the Gaussian is produced (see example below).

}
\examples{
	 
  g0 <- Gaussian(pi = 0.05, tau = 0.13)	
	
  g1 <- Gaussian(mu = 25, sigma = 8)
  g2 <- Gaussian(30, 6)
  g3 <- Multiply(g1, g2)

  # these are equivalent
  Gaussian$new(25, 8)
  Gaussian$new(mu = 25, pi = 8)
  Gaussian(mu = 25, pi = 8) 
  Gaussian(25, 8) 
  
  # approximately the same
  Gaussian(pi = 0.016, tau = 0.391) 
}
