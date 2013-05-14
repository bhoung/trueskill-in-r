library(trueskill)

# Example 1.
Alice  <- Player$new(rank = 1, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "1")
Bob    <- Player$new(rank = 2, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "2")
Chris  <- Player$new(rank = 2, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "3")
Darren <- Player$new(rank = 4, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "4")

players <- list(Alice, Bob, Chris, Darren)

# set default values for BETA, EPSILON and GAMMA where BETA is sigma / 2
# EPSILON is DrawProbability(0.1)
# GAMMA is sigma / 100

SetParameters()

players <- AdjustPlayers(players)  
print(players)
print(players[[1]]$skill)
print(Alice$skill)

# Example 1B

# note that AdjustPlayers sorts players by rank 
# as shown by inputting a list of different ordering
Alice  <- Player$new(rank = 1, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "1")
Bob    <- Player$new(rank = 2, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "2")
Chris  <- Player$new(rank = 2, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "3")
Darren <- Player$new(rank = 4, skill = Gaussian$new(mu = 25, sigma = 25 / 3), name = "4")

players <- list(Chris, Alice, Darren, Bob)
players <- AdjustPlayers(players)  
print(players)
print(players[[1]]$skill)


# Example 2
# Data format Player, Opponent, Margin, Round, WRank, LRank
#data <- read.csv("C:/Dropbox/2012 Aus Open.csv")
data("2012 Aus Open")

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
                                
data[c("mu1","sigma1")] <- c(300 - data$WRank, 300 - data$WRank - ((300 - data$WRank) / 3))
data[c("mu2","sigma2")] <- c(300 - data$LRank, 300 - data$LRank - ((300 - data$WRank) / 3)) 

data[!data$Round == "1st Round",][c("mu1","sigma1")] <- c(NA, NA)
data[!data$Round == "1st Round",][c("mu2","sigma2")] <- c(NA, NA)

# Expects columns mu1, sigma1, mu2 and sigma2, will set mu and sigma to 25 and 25 / 3 if NA.

SetParameters()
data <- trueskill(data)
top4 <- subset(data, Player == "Djokovic N." | Player == "Nadal R." | Player == "Federer R." | Player == "Murray A." )

library(ggplot2)
g1 <- ggplot(top4, aes(x = Round, y = mu1, group = Player, colour = Player)) + geom_point(aes(colour=factor(Player))) + geom_line(aes())       
g1

# Trueskill does not predict match outcomes well, as it appears that facing stiffer opposition (higher skilled players) tends to
# diminish a player's chances of progressing in the subsequent round.

# This is consistent with commentators describing players with softer draws and playing shorter matches (3 sets as opposed to 5 sets)
# as being fresher in later rounds.          

# The other feature is that the skill of the better players is weighted towards the losing player even if the
# better player wins, so we have this effect of the 4 semifinalists having their skills dropping as 
# the tournament progresses. This could be symptomatic of high starting values, which is necessary due to some of the 
# very low rankings. E.g Lleyton Hewitt with 181.
