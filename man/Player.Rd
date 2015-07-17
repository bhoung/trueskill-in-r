\name{Player}
\alias{Player}
\title{Player: class to hold the skills and names of players}
\description{
  Reference class to create objects that represent players.
}
\section{Usage:}{
  \describe{		
    Player(name, skill)
  }
}
\section{Arguments:}{
  \describe{
    \item{name}{name the player for display purposes}
    \item{skill}{skill of player represented by Gaussian object e.g. Gaussian(mu = 25, sigma = 25/3)}
  }  
}
\examples{
  Alice <- Player("Alice", Gaussian(25, 8))
  Bob <- Player("Bob", Gaussian(30, 7))
  players <- list(Alice, Bob)
  Print(players)
}

