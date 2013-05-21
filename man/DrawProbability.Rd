\name{DrawProbability}
\alias{DrawProbability}
\title{Compute draw probability}
\description{
Compute the draw probability given the draw margin (epsilon). Can be passed to DrawMargin to calculate EPSILON.
 Refer to \code{\link{Parameters}} for default input values.
}
\section{trueskill-package:}{
Refer to \code{\link{trueskill-package}} for links to related classes, functions and data.
}
\usage{
  DrawProbability(epsilon = 0.7404666, beta = 25 / 6, total_players = 2)
}
\arguments{
  \item{epsilon}{how common draws area}

  \item{beta}{randomness in game}

  \item{total_players}{number of players}
}
\examples{
  # draw_margin = 0.7404666 
  draw_margin <- DrawMargin(draw_probability = 0.10, beta = 25 / 6, total_players = 2)
  draw_probability = DrawProbability(epsilon = draw_margin, beta = 25 / 6, total_players = 2)		
}		
