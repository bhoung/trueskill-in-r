\name{GetPlayers}
\alias{GetPlayers}
\title{create a list of players from a list of teams}
\description{
  create a list of players from a list of teams
}
\usage{
  GetPlayers(teams)
}
\arguments{
  \item{teams}{a list of teams}
}
\examples{
  Alice <- Player("Alice", Gaussian(25, 8))
  Bob <- Player("Bob", Gaussian(30, 7))
  Team1 <- Team("Team1", rank = 1, list(Alice))
  Team2 <- Team("Team1", rank = 2, list(Bob))
  teams <- list(Team1, Team2)
  players <- GetPlayers(teams)
}

