\name{Player}
\alias{Player}
\title{Player: class to hold the rank, skill and names of players}
\description{
  Reference class to create objects that represent players.
}
\section{Usage:}{
  \describe{		
    Player(rank, skill, name)
  }
}
\section{Arguments:}{
  \describe{
    \item{rank}{rank of player in the match outcome}
    \item{skill}{skill of player represented by Gaussian object e.g. Gaussian(mu = 25, sigma = 25/3)}
    \item{name}{name the player for display purposes}
  }  
}
\examples{
  Alice <- Player(1, Gaussian(25, 8), "Alice")
  Bob <- Player(2, Gaussian(30, 7), "Bob")
  players <- list(Alice, Bob)
  PrintList(players)
}

