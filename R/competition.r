#' function Trueskill is to be applied to tournament data (two player head to head).  
#' @param data a data frame with columns: Player, Opponent, margin
#' @description Data is required to be in long format with two rows for each match, one with player 1 first and one with player 2 first
#' Matches should be sorted such that the second copy of the match appears in the second half of the dataframe
#' The package currently only supports the trueskill algorithm with one player per team
CompTrueskill <- function(data, parameters) {
	
  ApplyToRow <- function(row) {
    
    if(row$margin == 0) {
      rank1 = 1
      rank2 = 1
    } else {
      if(row$margin > 0) {
        rank1 = 1
        rank2 = 2
      } else {
        rank2 = 2
        rank2 = 1
      }
    }
  
    if(is.na(row$mu1) | is.na(row$sigma1) | is.na(row$mu2) | is.na(row$sigma2)) {
      row$mu1 <- 25                                
      row$sigma1 <- 25 / 3
      row$mu2 <- 25
      row$sigma2 <- 25 / 3 
    }
  
    Player1 <- Player(name = "P1", skill = Gaussian(mu = row$mu1, sigma = row$sigma1))
    Player2 <- Player(name = "P2", skill = Gaussian(mu = row$mu2, sigma = row$sigma2))
    
    Team1 <- Team(name = "Team1", rank = rank1, players = list(Player1))
    Team2 <- Team(name = "Team1", rank = rank2, players = list(Player2))
    
    teams <- Trueskill(list(Team1, Team2), parameters)
    players <- GetPlayers(teams)
    
    rm(Team1)
    rm(Team2)
    rm(Player1)
    rm(Player2)
                                          
    row$mu1 <- players[[1]]$skill$mu()
    row$sigma1 <- players[[1]]$skill$sigma()
    row$mu2 <- players[[2]]$skill$mu()
    row$sigma2 <- players[[2]]$skill$sigma()
    
    return(data.frame(row))  
  }
    
  N <- nrow(data) / 2
  for (i in (1:N)) {
     row <- ApplyToRow(data[i,])
     
     data[c("mu1", "sigma1", "mu2", "sigma2")][data$Player == row$Player & data$Round == row$Round,] <- row[c("mu1", "sigma1", "mu2", "sigma2")]
     data[c("mu1", "sigma1", "mu2", "sigma2")][data$Opponent == row$Player & data$Round == row$Round,] <- row[c("mu2", "sigma2", "mu1", "sigma1")]
     
     # index of the round we are up to
     next_round <- which(row$Round == levels(data$Round)) + 1
     
     if (next_round <= length(levels(data$Round))) {
  
       next_round_value <- levels(data$Round)[next_round]
       data[c("mu1", "sigma1", "mu2", "sigma2")][data$Player == row$Player & data$Round == next_round_value,] <- row[c("mu1", "sigma1", "mu2", "sigma2")]
       data[c("mu1", "sigma1", "mu2", "sigma2")][data$Opponent == row$Player & data$Round == next_round_value,] <- row[c("mu2", "sigma2", "mu1", "sigma1")]
       
     }   
  }
  
  data$mu1 <- round(data$mu1, 1)
  data$sigma1 <- round(data$sigma1, 1)
  data$mu2 <- round(data$mu2, 1)
  data$sigma2 <- round(data$sigma2, 1)
  
  return(data)           
} 
