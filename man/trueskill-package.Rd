\name{trueskill-package}
\alias{trueskill-package}
\alias{trueskill}
\docType{package}
\title{
Implementation the Bayesian TrueSkill algorithm
}
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
    Built: \tab R 3.0.0; ; 2013-05-14 02:24:46 UTC; windows\cr
  }
}

\section{Functions and Classes/Objects}{

  \subsection{\code{Gaussian}}{
    Skills are represented by \code{'Gaussian'} objects. E.g. Gaussian$new(mu = 0, sigma = 1).
  }                                            
  \subsection{\code{Player}}{
  Create Player objects with \code{'Player$new(rank, skill, name)'} by setting mu, sigma. Input name is optional.
  
    \code{Player1 <- Player$new(rank = 1, Gaussian$new(mu = 25, sigma = 25/3 ), name = "Alice")}
    
    or setting pi and tau:
    
    \code{Player1 <- Player$new(rank = 1, Gaussian$new(pi = 0.014, tau = 0.36), name = "Bob")}
    
  }
  \subsection{SetParameters()}{
  Set global parameters: beta, the randomness of the game, epsilon, the probability of a draw, and gamma the amount of uncertainty in each player's
  skill prior to each match.  
    
    \code{SetParameters(beta, draw_probability, gamma)}
    
    or
    
    \code{SetParameters()}
    
    which assumes initial beta of sigma / 2, draw_probability of 0.10, 
    and gamma or sigma / 100.
    
  }
  \subsection{AdjustPlayers(list_of_players)}{
  \code{AdjustPlayers} takes an input of a list of players and runs the TrueSkill algorithm. It returns the list of players with their skill's updated.
  Note that the function sorts the list of players by their rank, and returns list sorted.
  
  \code{players <- list(Alice, Bob, Chris, Darren)}
  
  \code{players <- AdjustPlayers(players)}
  }
  
}
\author{
  Brendan Houng <brendan.houng@gmail.com> \cr
}
\references{
  [1] TrueSkill: A Bayesian Skill Rating System, Herbrich, R., Minka, T. and Grapel, T.
  
  [2] Doug Zongker's python implementation of TrueSkill: https://github.com/dougz/trueskill
  
  [3] Heungsub Lee's python implementation of TrueSkill: https://github.com/sublee/trueskill.
  
  [4] Jeff Moser's explanatory notes 
}
\keyword{ package }
\keyword{ trueskill }
\examples{
	
  # Example 1.
  
  # set default values for BETA, EPSILON and GAMMA where BETA is sigma / 2
  # EPSILON is DrawProbability(0.1)
  # GAMMA is sigma / 100
  
  SetParameters()
  
  Alice  <- Player$new(rank = 1, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "1")
  Bob    <- Player$new(rank = 2, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "2")
  Chris  <- Player$new(rank = 2, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "3")
  Darren <- Player$new(rank = 4, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "4") 
   
  players <- list(Alice, Bob, Chris, Darren)
  
  players <- AdjustPlayers(players)  
  PrintList(players)
  print(Alice$skill)

  # Relying on positional arguments looks much cleaner:
  Alice  <- Player(1, Gaussian(25, 8.3), "Alice")
  Bob    <- Player(2, Gaussian(25, 8.3), "Bob")
  Chris  <- Player(2, Gaussian(25, 8.3), "Chris")
  Darren <- Player(4, Gaussian(25, 8.3), "Darren") 
  
  # Example 2
  # This second example runs Trueskill on a tennis tournament, the Australian Open.
  # Data format Player, Opponent, Margin, Round, WRank, LRank
  data("ausopen2012")
  
  # create match_id in order to reshape
  data$match_id <- row.names(data)
  
  # reshape wide to long on match_id such that we have
  # 2 rows per match, 1 with Player1 as Player and 1 with 
  # Player2 as Opponent and vice versa.
  
  data <- data[c("Winner", "Loser", "Round", "WRank", "LRank")]
  data <- reshape(data,
    idvar = "match_id",
    varying = list(c(1, 2), c(2, 1), c(4, 5), c(5,4)),
    v.names = c("Player", "Opponent", "WRank", "LRank"),
    new.row.names = 1:1000, 
    timevar = "t",
    direction = "long")
    
  # data comes preformatted with winner in Player column
  # set margin to 1 for win and -1 for loss.
  
  data$margin[data$t == "1"] <- 1
  data$margin[data$t != "1"] <- -1
  data$t <- NULL
  
  data$mu1 <- NA
  data$sigma1 <- NA
  data$mu2 <- NA
  data$sigma2 <- NA
  
  # For the first round, set Mu to 300 less the ATP rank
  # Skill tends to be stable at the higher rankings (descending from 1), so set sigma at mu less mu / 3, 
  # rather than the recommended mu / 3
                                  
  data[c("mu1","sigma1")] <- c(300 - data$WRank, round(300 - data$WRank - ((300 - data$WRank) / 3), 1))
  data[c("mu2","sigma2")] <- c(300 - data$LRank, round(300 - data$LRank - ((300 - data$WRank) / 3), 1)) 
  
  data[!data$Round == "1st Round",][c("mu1","sigma1")] <- c(NA, NA)
  data[!data$Round == "1st Round",][c("mu2","sigma2")] <- c(NA, NA)
  
  # Expects columns mu1, sigma1, mu2 and sigma2, will set mu and sigma to 25 and 25 / 3 if NA.
  
  SetParameters()
  data <- Trueskill(data)
  top4 <- subset(data, Player == "Djokovic N." | Player == "Nadal R." | Player == "Federer R." | Player == "Murray A." )
  top4 <- top4[order(top4$Player,top4$Round),]
  
  subset(top4, Player == "Djokovic N.")      
  
  # For a visualisation, load up our favourite package ggplot2...	
  # library(ggplot2)
  # g1 <- ggplot(top4, aes(x = Round, y = mu1, group = Player, colour = Player)) + geom_point(aes(colour=factor(Player))) + geom_line(aes())       
  # g1
  
  # Without having adjusted the global parameters, Trueskill does not predict match outcomes well,
  # as it appears that facing stiffer opposition (higher skilled players) tends to
  # diminish a player's chances of progressing in the subsequent round.
  
  # This is consistent with commentators describing players with softer draws and playing shorter matches (3 sets as opposed to 5 sets)
  # as being fresher in later rounds.          
  
  # The other feature is that the skill of the better players is weighted towards the losing player even if the
  # better player wins, so we have this effect of the 4 semifinalists having their skills dropping as 
  # the tournament progresses. This could be symptomatic of low starting values, which is necessary due to some of the 
  # very low rankings. E.g Lleyton Hewitt with 181.
  
}
