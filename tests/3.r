test.examples <- function() {

epsilon <- DrawMargin(draw_probability = 0.1, beta = 25 / 6, num_teams = 4)
parameters <- Parameters(beta = 25/6, epsilon, 25 / 300)

Alice  <- Player(name = "1", skill = Gaussian(mu = 25, sigma = 25 / 3))
Bob    <- Player(name = "2", skill = Gaussian(mu = 25, sigma = 25 / 3))
Chris  <- Player(name = "3", skill = Gaussian(mu = 25, sigma = 25 / 3))
Darren <- Player(name = "4", skill = Gaussian(mu = 25, sigma = 25 / 3)) 

Team1 <- Team(name = "Team Alice", rank = 1, players = list(Alice))
Team2 <- Team(name = "Team Bob", rank = 1, players = list(Bob))
Team3 <- Team(name = "Team Chris", rank = 1, players = list(Chris))
Team4 <- Team(name = "Team Darren", rank = 1, players = list(Darren))

teams <- list(Team1, Team2, Team3, Team4)
teams <- Trueskill(teams, parameters) 

Print(teams)

}

test.deactivation <- function() {
  DEACTIVATED('Deactivating this test function')
}
