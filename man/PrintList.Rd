\name{PrintList}
\alias{PrintList}
\title{pretty print a list of players}
\description{
  pretty print a list of players (trivial function...)
}
\usage{
  PrintList(list)
}
\arguments{
  \item{list}{a list of player objects}
}
\examples{
  Alice <- Player(1, Gaussian(25, 8), "Alice")
  Bob <- Player(2, Gaussian(30, 7), "Bob")
  players <- list(Alice, Bob)
  PrintList(players)
}

