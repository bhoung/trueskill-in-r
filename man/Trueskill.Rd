\name{Trueskill}
\alias{Trueskill}
\title{Runs the Trueskill algorithm}
\description{
  Runs the trueskill algorithm and updates the skills of players within each team.
}
\section{Usage:}{ 
\describe{
  
  Trueskill(teams, parameters)
  
  Trueskill(list(Team1, Team2), parameters)
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
}
}
\examples{
  # set default values for BETA, EPSILON and GAMMA where BETA is sigma / 2
  # EPSILON is DrawProbability(0.1)
  # GAMMA is sigma / 100
  epsilon <- DrawMargin(draw_probability = 0.1, beta = 25 / 6, num_teams = 4)
  parameters <- Parameters(beta = 25/6, epsilon, 25 / 300)
  
  Alice  <- Player(name = "1", skill = Gaussian(mu = 25, sigma = 25 / 3))
  Bob    <- Player(name = "2", skill = Gaussian(mu = 25, sigma = 25 / 3))
  Chris  <- Player(name = "3", skill = Gaussian(mu = 25, sigma = 25 / 3))
  Darren <- Player(name = "4", skill = Gaussian(mu = 25, sigma = 25 / 3)) 
   
  Team1 <- Team(name = "Team Alice", rank = 1, players = list(Alice))
  Team2 <- Team(name = "Team Bob", rank = 2, players = list(Bob))
  Team3 <- Team(name = "Team Chris", rank = 2, players = list(Chris))
  Team4 <- Team(name = "Team Darren", rank = 4, players = list(Darren))

  teams <- list(Team1, Team2, Team3, Team4)
  teams <- Trueskill(teams, parameters) 
  PrintList(teams)
  
  players <- GetPlayers(teams)
  Alice <- players[[1]]
  print(Alice$skill) 		
}

