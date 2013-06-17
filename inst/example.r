#dropbox = "C:/Dropbox/"
dropbox = "/Users/brendanhoung/Dropbox/"

source(paste(dropbox, "trueskill-in-r/R/competition.r", sep = ""))
source(paste(dropbox, "trueskill-in-r/R/factorgraph.r", sep = ""))
source(paste(dropbox, "trueskill-in-r/R/player.r", sep = ""))
source(paste(dropbox, "trueskill-in-r/R/team.r", sep = ""))
source(paste(dropbox, "trueskill-in-r/R/init.r", sep = ""))

# Example 1.

Alice  <- Player(name = "Alice",  skill = Gaussian(mu = 25, sigma = 25 / 3))
Bob    <- Player(name = "Bob",    skill = Gaussian(mu = 25, sigma = 25 / 3))
Chris  <- Player(name = "Chris",  skill = Gaussian(mu = 25, sigma = 25 / 3))
Darren <- Player(name = "Darren", skill = Gaussian(mu = 25, sigma = 25 / 3))

Team1 <- Team(name = "1", rank = 1, list(Alice))
Team2 <- Team(name = "2", rank = 2, list(Bob, Chris))
Team3 <- Team(name = "3", rank = 3, list(Darren))
teams <- list(Team1, Team2, Team3)

epsilon <- DrawMargin(draw_probability = 0.1, beta = 25 / 6, num_teams = 3)
parameters <- Parameters(beta = 25/6, epsilon, 25 / 300)

teams <- Trueskill(teams, parameters)

PrintPlayers(teams)

# Example 2.

# Example 2 applies the Trueskill algorithm to a tennis tournament, the Australian Open. 
# As it is applied to 127 matches, it takes ~40 seconds to run.

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
print(parameters)

data <- CompTrueskill(data, parameters)
# top4 <- subset(data, Player == "Djokovic N." | Player == "Nadal R." | Player == "Federer R." | Player == "Murray A." )
# top4 <- top4[order(top4$Player,top4$Round),]

# subset(top4, Player == "Djokovic N.")      

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
# the tournament progresses. This could be symptomatic of high starting values, which is necessary due to some of the 
# very low rankings. E.g Lleyton Hewitt with 181.


