\name{Trueskill}
\alias{Trueskill}
\title{Runs the Trueskill algorithm}
\description{
  Runs the trueskill algorithm and updates the skills of players within each team.
  Can be applied to a list of teams or to a dataframe
  
  1) Trueskill applied to a list - see example 
  
  2) Trueskill applied to tournament data in dataframe format.
  
  Data is required to be in long format with two rows for 
  each match, one with player 1 first and one with player 2 
  first. 
  
  Matches should be sorted such that the second copy
  of the match appears in the second half of the dataframe. 
  
  Should this not match  the data you are interested, a function could be written from  
  Trueskill (list), Team, Player and Gaussian, and Parameters.
  
  An example is provided at \url{https://gist.github.com/bhoung/5596282}
  E.g. library(devtools) 
       library(trueskill)
       source_gist("https://gist.github.com/bhoung/5596282")

  (runtime is approx 50 secs).
}
\section{Usage}{ 
\describe{
	
  ## S3 Method for class list
  Trueskill(teams, parameters)
  Trueskill(list(Team1, Team2), parameters)
  
  ## S3 Method for class dataframe
  Trueskill(data, parameters)

}
}

\section{Arguments:}{
\describe{
  \item{teams}{is a list of teams objects, for all the
  teams that participated in a single game. 

  This function updates all the skills of the
  player objects to reflect the outcome of the game. 
  Note the function sorts the teams by rank and returns
  the sorted list. Creates all the variable nodes
  in the graph.}
  
  \item{parameters}{Parameters object to hold input variables: \cr
  beta, epsilon and gamma.
  See Parameters for more details.}
  
  \item{data}{a data frame (\code{\link{data}}) with columns: Player, Opponent, margin.}
}
}

\examples{
  # set default values for BETA, EPSILON and GAMMA where BETA is sigma / 2
  # EPSILON is DrawProbability(0.1)
  # GAMMA is sigma / 100
  epsilon <- DrawMargin(draw_probability = 0.1, beta = 25 / 6, num_teams = 4)
  parameters <- Parameters(beta = 25/6, epsilon, 25 / 300)
  
  Alice  <- Player(name = "Alice", skill = Gaussian(mu = 25, sigma = 25 / 3))
  Bob    <- Player(name = "Bob", skill = Gaussian(mu = 25, sigma = 25 / 3))
  Chris  <- Player(name = "Chris", skill = Gaussian(mu = 25, sigma = 25 / 3))
  Darren <- Player(name = "Darren", skill = Gaussian(mu = 25, sigma = 25 / 3)) 
   
  Team1 <- Team(name = "Team Alice", rank = 1, players = list(Alice))
  Team2 <- Team(name = "Team Bob", rank = 2, players = list(Bob))
  Team3 <- Team(name = "Team Chris", rank = 2, players = list(Chris))
  Team4 <- Team(name = "Team Darren", rank = 4, players = list(Darren))

  teams <- list(Team1, Team2, Team3, Team4)
  teams <- Trueskill(teams, parameters) 
  Print(teams)
  
  players <- GetPlayers(teams)
  Print(players)
  
  Alice <- players[[1]]
  print(Alice$skill)
  
}

