\name{trueskill-package}
\alias{trueskill-package}
\alias{trueskill}
\docType{package}
\title{ Implementation of the TrueSkill algorithm }
\description{
	
  An R implementation of the TrueSkill Algorithm (Herbrich, R., Minka, T. and Grapel, T. [1]),    
  a Bayesian skill rating system with inference by approximate message passing on a factor graph.
  Used by Xbox to rank gamers and identify appropriate matches.
  
  http://research.microsoft.com/en-us/projects/trueskill/default.aspx
  
  Current version allows for one player per team. Will update as time permits. 
  Requires R version 3.0 as it is implemented with Reference Classes.
  
  The code for the examples can be found at: 
  
  \code{system.file('', package = 'trueskill')}

  Acknowledgements to Doug Zongker [2] and Heungsub Lee [3] for their python implementations 
  of the algorithm and for the liberal reuse of Doug's code comments. 

}
\details{
  \tabular{ll}{
    Package: \tab trueskill\cr
    URL: \tab http://www.bhoung.com/trueskill\cr
    Version: \tab 0.1\cr
    License: \tab Apache\cr
    Depends: \tab R (>= 3.0)\cr
    Built: \tab R 3.0.1 \cr
  }
}
\section{Main Functions and Classes}{
  \describe{
    \item{Reference Classes:}{ \code{\link{Gaussian}}, 
                               \code{\link{Player}}, 
                               \code{\link{Parameters}}}
    \item{Methods:}{\code{\link{Multiply}}, 
                    \code{\link{Divide}}} 
    \item{Functions:}{\code{\link{AdjustPlayers}}, 
                      \code{\link{Trueskill}},
                      \code{\link{DrawMargin}},
                      \code{\link{DrawProbability}},
                      \code{\link{PrintList}}
    }
    \item{Data:}{\code{\link{data}}}
  }
}
\author{
  Brendan Houng <brendan.houng@gmail.com> \cr
}
\references{
  \describe{		
    \item{[1]}{TrueSkill: A Bayesian Skill Rating System, Herbrich, R., Minka, T. and Grapel, T.}
    
    \item{[2]}{Doug Zongker's python implementation: \cr
    \url{https://github.com/dougz/trueskill}}
    
    \item{[3]}{ Heungsub Lee's python implementation: \cr
    \url{https://github.com/sublee/trueskill}.}
    
    \item{[4]}{ Jeff Moser's explanatory notes: \cr
    \url{http://www.moserware.com/2010/03/computing-your-skill.html}}
  }
}
\keyword{ package }
\keyword{ trueskill }
\examples{
  # Example 1.
  
  # set default values for BETA, EPSILON and GAMMA where BETA is sigma / 2
  # EPSILON is DrawProbability(0.1)
  # GAMMA is sigma / 100
  parameters <- Parameters$new()
  
  Alice  <- Player(rank = 1, skill = Gaussian(mu = 25, sigma = 25 / 3), name = "1")
  Bob    <- Player(rank = 2, skill = Gaussian(mu = 25, sigma = 25 / 3), name = "2")
  Chris  <- Player(rank = 2, skill = Gaussian(mu = 25, sigma = 25 / 3), name = "3")
  Darren <- Player(rank = 4, skill = Gaussian(mu = 25, sigma = 25 / 3), name = "4") 
   
  players <- list(Alice, Bob, Chris, Darren)
  
  players <- AdjustPlayers(players, parameters)  
  PrintList(players)
  print(Alice$skill)

  # Relying on positional arguments looks much cleaner:
  Alice  <- Player(1, Gaussian(25, 8.3), "Alice")
  Bob    <- Player(2, Gaussian(25, 8.3), "Bob")
  Chris  <- Player(2, Gaussian(25, 8.3), "Chris")
  Darren <- Player(4, Gaussian(25, 8.3), "Darren") 
 
  # Example 2 - see https://gist.github.com/bhoung/5596282  
  # the example applies trueskill to tennis tournament data
  # (runtime is approx 50 secs)
  
}
