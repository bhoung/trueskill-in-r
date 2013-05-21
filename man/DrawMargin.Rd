\name{DrawMargin}
\alias{DrawMargin}
\title{EPSILON or draw margin, used to set EPSILON in Parameters}

\description{
  Compute EPSILON or draw margin, a measure of how likely a
  draw is. Then pass to Parameters object.
  
  Takes draw probability as input. Refer to \code{\link{Parameters}} for default input values.
}
\usage{
  DrawMargin(draw_probability = 0.10, beta = 25 / 6, total_players = 2)
}
\arguments{
  \item{draw_probability}{draw probability}

  \item{beta}{randomness in game}

  \item{total_players}{number of players}
}  
\examples{
  draw_margin <-DrawMargin(draw_probability = 0.10, beta = 25 / 6, total_players = 2)
  parameters <- Parameters(beta = 25 / 6, epsilon = draw_margin, gamma = 25 / 300)
}
