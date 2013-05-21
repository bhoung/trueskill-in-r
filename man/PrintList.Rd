\name{PrintList}
\alias{PrintList}
\title{pretty print a list of players}
\usage{
  PrintList(list)
}
\arguments{
  \item{list}{a list of player objects}
}
\description{
  pretty print a list of players (trivial function...)
}
\examples{
  Alice <- Player(1, Gaussian(25, 8), "Alice")
  Bob <- Player(2, Gaussian(30, 7), "Bob")
  players <- list(Alice, Bob)
  PrintList(players)
}

