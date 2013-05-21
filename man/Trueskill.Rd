\name{Trueskill}
\alias{Trueskill}
\title{Apply Trueskill to Tournament Data}
\description{
  Trueskill function to be applied to tournament data in dataframe format.
  
  Data is required to be in long format with two rows for
  each match, one with player 1 first and one with player 2
  first Matches should be sorted such that the second copy
  of the match appears in the second half of the dataframe.
    
  The package currently only supports the trueskill
  algorithm with one player per team.
  
  Should this not match the data you are interested, a function could be written from
  AdjustPlayers, Player and Gaussian, and SetParameters.
  
}
\section{trueskill-package:}{
Refer to \code{\link{trueskill-package}} for links to related classes, functions and data.
}
\usage{
  Trueskill(data, parameters)
}
\arguments{
  \item{data}{a data frame with columns: Player, Opponent, margin}
  \item{parameters}{Parameters object to hold input variables: beta, epsilon and gamma.
  See \code{\link{Parameters}} for more details.
  }
}
\examples{
  # This second example runs Trueskill on a tennis tournament, the Australian Open.
  # Note that actual computation is commented out as it takes about ~40 seconds to 
  # update skill ratings over 127 matches.
  
  # Data format of ausopen2012 is: Player, Opponent, Margin, Round, WRank, LRank
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
  
  parameters <- Parameters()
  # data <- Trueskill(data, parameters)
  # top4 <- subset(data, Player == "Djokovic N." | Player == "Nadal R." | Player == "Federer R." | Player == "Murray A." )
  # top4 <- top4[order(top4$Player,top4$Round),]
  
  # subset(top4, Player == "Djokovic N.")      
  
  # For a visualisation, load up our favourite package ggplot2...	
  # library(ggplot2)
  # g1 <- ggplot(top4, aes(x = Round, y = mu1, group = Player, colour = Player)) + geom_point(aes(colour=factor(Player))) + geom_line(aes())       
  # g1
  
  # Without having adjusted the input parameters, Trueskill does not predict match outcomes well,
  # as it appears that facing stiffer opposition (higher skilled players) tends to
  # diminish a player's chances of progressing in the subsequent round.
  
  # This is consistent with commentators describing players with softer draws and playing shorter matches (3 sets as opposed to 5 sets)
  # as being fresher in later rounds.          
  
  # The other feature is that the skill of the better players is weighted towards the losing player even if the
  # better player wins, so we have this effect of the 4 semifinalists having their skills dropping as 
  # the tournament progresses. This could be symptomatic of high starting values, which is necessary due to some of the 
  # very low rankings. E.g Lleyton Hewitt with 181.
 	
}
