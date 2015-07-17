\name{Print}
\alias{Print}
\title{pretty print a list ( of players or teams)}
\description{
  pretty print a list (of players or teams)
}
\usage{
  Print(list)
}
\arguments{
  \item{list}{a list of players or teams}
}
\examples{
  Alice <- Player("Alice", Gaussian(25, 8))
  Bob <- Player("Bob", Gaussian(30, 7))
  players <- list(Alice, Bob)
  Print(players)
  
  Team1 <- Team(name = "Team Alice", rank = 1, players = list(Alice))
  Team2 <- Team(name = "Team Bob", rank = 2, players = list(Bob))
  teams <- list(Team1, Team2)
  Print(teams)

}

