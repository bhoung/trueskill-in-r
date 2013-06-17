\name{Team}
\alias{Team}
\title{Team: class to hold the names, ranks and list of players}
\description{
  Reference class to create objects that represent teams.
}
\section{Usage:}{
  \describe{		
    Team(name, rank, players)
  }
}
\section{Arguments:}{
  \describe{
    \item{name}{name the team for display purposes}
    \item{rank}{rank of team in the match outcome}
    \item{players}{a list of players belonging to this team}
  }  
}
\examples{
  Alice <- Player("Alice", Gaussian(25, 8))
  Bob <- Player("Bob", Gaussian(30, 7))
  players <- list(Alice, Bob)
  Team1 <- Team("Team1", rank = 1, players)
}

