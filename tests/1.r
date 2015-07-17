#Test 1

# default inputs are
# INITIAL_MU = 25.0
# INITIAL_SIGMA = INITIAL_MU / 3.0
# INITIAL_BETA = INITIAL_SIGMA / 2.0                   
# INITIAL_GAMMA = INITIAL_SIGMA / 100.0
# DRAW_PROBABILITY = 0.10
# INITIAL_EPSILON = DrawMargin(DRAW_PROBABILITY, BETA)
  
test.examples <- function() {
  Alice  <- Player(name = "Alice",  skill = Gaussian(mu = 25, sigma = 25 / 3))
  Bob    <- Player(name = "Bob",    skill = Gaussian(mu = 25, sigma = 25 / 3))
  Chris  <- Player(name = "Chris",  skill = Gaussian(mu = 25, sigma = 25 / 3))
  Darren <- Player(name = "Darren", skill = Gaussian(mu = 25, sigma = 25 / 3))
  
  Team1 <- Team(name = "1", rank = 1, list(Alice))
  Team2 <- Team(name = "2", rank = 2, list(Bob, Chris))
  Team3 <- Team(name = "3", rank = 3, list(Darren))
  teams <- list(Team1, Team2, Team3)
  PrintPlayers(teams)
  
  epsilon <- DrawMargin(draw_probability = 0.1, beta = 25 / 6, num_teams = 3)
  parameters <- Parameters(beta = 25/6, epsilon, 25 / 300)
  
  teams <- Trueskill(teams, parameters)
  
  Print(teams)
  
  players <- GetPlayers(teams)
        
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2, 35.877, round(players[[1]]$skill$mu(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2,  6.791, round(players[[1]]$skill$sigma(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2, 17.867, round(players[[2]]$skill$mu(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2,  7.059, round(players[[2]]$skill$sigma(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2, 17.867, round(players[[3]]$skill$mu(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2,  7.059, round(players[[3]]$skill$sigma(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2, 21.256, round(players[[4]]$skill$mu(), 3))
  checkEqualsNumeric(tolerance = .Machine$double.eps^0.2,  7.155, round(players[[4]]$skill$sigma(), 3)) 
  
}

test.deactivation <- function() {
  DEACTIVATED('Deactivating this test function')
}
