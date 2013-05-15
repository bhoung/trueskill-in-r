\name{SetParameters}
\alias{SetParameters}
\title{Sets three global parameters used in the TrueSkill algorithm.}
\usage{
  SetParameters(beta = INITIAL_BETA, draw_probability = DRAW_PROBABILITY, gamma = INITIAL_GAMMA)
}
\arguments{
  \item{beta}{is a measure of how random the game is. }

  \item{draw_probability}{ probability that game ends in a draw. 
  Can be calculated from DrawProbability function.}
  	 
  \item{gamma}{is a small amount by which a player's
  uncertainty (sigma) is increased prior to the start of
  each game. }
}
\description{
  Sets three global parameters used in the TrueSkill algorithm.
}

