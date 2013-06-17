\name{PrintList}
\alias{PrintList}
\title{pretty print a list ( of players or teams)}
\description{
  pretty print a list (of players or teams)
}
\usage{
  PrintList(list)
}
\arguments{
  \item{list}{a list of players or teams}
}
\examples{
  Alice <- Player("Alice", Gaussian(25, 8))
  Bob <- Player("Bob", Gaussian(30, 7))
  players <- list(Alice, Bob)
  PrintList(players)
}

