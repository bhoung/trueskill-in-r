\name{Parameters}
\alias{Parameters}
\title{Sets three parameters used in the TrueSkill algorithm.}
\description{
  Class creates an object to hold three parameters used in the TrueSkill algorithm.
  Passed to AdjustPlayer and Trueskill to perform calculations and update the Player objects or data.
%  
The default parameters object is:
%
"Parameters [(beta, epilson, gamma)]: [(4.167, 0.74, 0.083)]"
%
where the default inputs are:
%
INITIAL_MU = 25.0 
%
INITIAL_SIGMA = INITIAL_MU / 3.0 
%
INITIAL_BETA = INITIAL_SIGMA / 2.0 
%                   
INITIAL_GAMMA = INITIAL_SIGMA / 100.0 
%
DRAW_PROBABILITY = 0.10 
%
INITIAL_EPSILON = DrawMargin(DRAW_PROBABILITY, BETA) 
}
\section{Usage:}{
  \describe{		
    Parameters(beta, epsilon, gamma)
  }
}

\section{Arguments:}{
  \describe{
    \item{beta}{is a measure of how random the game is. }
    
    \item{draw_probability}{ probability that game ends in a draw. 
    Can be calculated from DrawProbability function.}
    	 
    \item{gamma}{is a small amount by which a player's
    uncertainty (sigma) is increased prior to the start of
    each game. }
  }
}
\examples{
  parameters <- Parameters()
  
  # alternatively and equiavelently
  draw_margin <-DrawMargin(draw_probability = 0.10, beta = 25 / 6, total_players = 2)
  parameters <- Parameters(beta = 25 / 6, epsilon = draw_margin, gamma = 25 / 300)
}		
