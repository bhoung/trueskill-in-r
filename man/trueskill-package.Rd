\name{trueskill-package}
\alias{trueskill-package}
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

  Html documentation is at http://www.bhoung.com/trueskill/
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
    \item{Functions:}{\code{\link{Trueskill}},
                      \code{\link{DrawMargin}},
                      \code{\link{DrawProbability}},
                      \code{\link{Print}}
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
  epsilon <- DrawMargin(draw_probability = 0.1, beta = 25 / 6, num_teams = 4)
  parameters <- Parameters(beta = 25/6, epsilon, 25 / 300)
  
  Alice  <- Player(name = "Alice", skill = Gaussian(mu = 25, sigma = 25 / 3))
  Bob    <- Player(name = "Bob", skill = Gaussian(mu = 25, sigma = 25 / 3))
  Chris  <- Player(name = "Chris", skill = Gaussian(mu = 25, sigma = 25 / 3))
  Darren <- Player(name = "Darren", skill = Gaussian(mu = 25, sigma = 25 / 3)) 
   
  Team1 <- Team(name = "Team Alice", rank = 1, players = list(Alice))
  Team2 <- Team(name = "Team Bob", rank = 2, players = list(Bob))
  Team3 <- Team(name = "Team Chris", rank = 2, players = list(Chris))
  Team4 <- Team(name = "Team Darren", rank = 4, players = list(Darren))

  teams <- list(Team1, Team2, Team3, Team4)
  teams <- Trueskill(teams, parameters) 
  Print(teams)
  
  players <- GetPlayers(teams)
  Print(players)

  Alice <- players[[1]]
  print(Alice$skill) 

  # Example 2 - see https://gist.github.com/bhoung/5596282  
  # the example applies trueskill to tennis tournament data
  # (runtime is approx 50 secs)
  
}
