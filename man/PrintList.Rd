\name{PrintList}
\alias{PrintList}
\title{display list of players nicer}
\usage{
  PrintList(list)
}
\arguments{
  \item{list}{a list of player objects}
}
\description{
  display list of players nicer
}
\examples{
  Alice <- Player(1, Gaussian(25, 8), "Alice")
  Bob <- Player(2, Gaussian(30, 7), "Bob")
  players <- list(Alice, Bob)
  PrintList(players)
}

