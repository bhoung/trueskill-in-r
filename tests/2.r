# Test 2.

# default inputs are 
# INITIAL_MU = 25.0
# INITIAL_SIGMA = INITIAL_MU / 3.0
# INITIAL_BETA = INITIAL_SIGMA / 2.0                   
# INITIAL_GAMMA = INITIAL_SIGMA / 100.0
# DRAW_PROBABILITY = 0.10
# INITIAL_EPSILON = DrawMargin(DRAW_PROBABILITY, BETA)

test.examples <- function() {
  source(paste(dropbox, "trueskill-in-r/R/competition.r", sep = ""))
  source(paste(dropbox, "trueskill-in-r/R/factorgraph.r", sep = ""))
  source(paste(dropbox, "trueskill-in-r/R/player.r", sep = ""))
  source(paste(dropbox, "trueskill-in-r/R/team.r", sep = ""))
  source(paste(dropbox, "trueskill-in-r/R/init.r", sep = ""))
  
  Alice  <- Player(name = "Alice",  skill = Gaussian(mu = 25, sigma = 25 / 3))
  Bob    <- Player(name = "Bob",    skill = Gaussian(mu = 25, sigma = 25 / 3))
  Chris  <- Player(name = "Chris",  skill = Gaussian(mu = 25, sigma = 25 / 3))
  Darren <- Player(name = "Darren", skill = Gaussian(mu = 25, sigma = 25 / 3))

  Team1 <- Team(name = "1", rank = 1, list(Alice))
  Team2 <- Team(name = "2", rank = 2, list(Bob))
  Team3 <- Team(name = "3", rank = 2, list(Darren))
  Team4 <- Team(name = "4", rank = 3, list(Chris))
  
  teams <- list(Team1, Team2, Team3, Team4)
  
  PrintPlayers(teams)
  
  epsilon <- DrawMargin(draw_probability = 0.1, beta = 25 / 6, num_teams = 4)
  parameters <- Parameters(beta = 25/6, epsilon, 25 / 300)
  
  teams <- Trueskill(teams, parameters)
  players <- GetPlayers(teams)
  
  print(players)
  
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2, 31.716, round(players[[1]]$skill$mu(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2,  6.386, round(players[[1]]$skill$sigma(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2, 24.986, round(players[[2]]$skill$mu(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2,  5.556, round(players[[2]]$skill$sigma(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2, 25.014, round(players[[3]]$skill$mu(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2,  5.556, round(players[[3]]$skill$sigma(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2, 18.284, round(players[[4]]$skill$mu(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2,  6.386, round(players[[4]]$skill$sigma(), 3))
  
  PrintPlayers(teams)
}

test.deactivation <- function() {
  DEACTIVATED('Deactivating this test function')
}
