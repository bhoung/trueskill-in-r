\name{AdjustPlayers}
\alias{AdjustPlayers}
\title{Update the Skills of a List of Players}
\description{
  Runs the trueskill algorithm and updates the skills of a list of players.
  Assumes each team has one player.
}
\section{Usage:}{ 
\describe{
  
  AdjustPlayers(players, parameters)
  
  AdjustPlayers(list(Alice, Bob), parameters)
}
}
\section{Arguments:}{
\describe{
  \item{players}{is a list of player objects, for all the
  players who participated in a single game. A 'player
  object' is any object with a 'skill' field (a
  Gaussian) and a 'rank' field. Lower ranks are better; the
  lowest rank is the overall winner of the game.  Equal
  ranks mean that the two players drew.

  This function updates all the skills of the
  player objects to reflect the outcome of the game. 
  Not the function sorts the players by rank and returns
  the sorted list. Creates all the variable nodes
  in the graph.  "Teams" are each a single player; there's
  a one-to-one correspondence between players and teams.}
  
  \item{parameters}{Parameters object to hold input variables: \cr
  beta, epsilon and gamma.
  See Parameters for more details.}
}
}
\examples{
  Alice  <- Player(rank = 1, skill = Gaussian(mu = 25, sigma = 25 / 3), name = "1")
  Bob    <- Player(rank = 2, skill = Gaussian(mu = 25, sigma = 25 / 3), name = "2")
  Chris  <- Player(rank = 2, skill = Gaussian(mu = 25, sigma = 25 / 3), name = "3")
  Darren <- Player(rank = 4, skill = Gaussian(mu = 25, sigma = 25 / 3), name = "4") 
   
  players <- list(Alice, Bob, Chris, Darren)
  
  parameters <- Parameters$new()
  players <- AdjustPlayers(players, parameters) 		
}

