\name{PrintPlayers}
\alias{PrintPlayers}
\title{pretty print players from a list of teams}
\description{
  pretty print players from a list of teams
}
\usage{
  PrintPlayers(list)
}
\arguments{
  \item{list}{a list of teams objects}
}
\examples{
  Alice <- Player("Alice", Gaussian(25, 8))
  Bob <- Player("Bob", Gaussian(30, 7))
  Team1 <- Team("T1", rank = 1, list(Alice))
  Team2 <- Team("T2", rank = 2, list(Bob))
  
  teams <- list(Team1, Team2)
  PrintPlayers(teams)
}

