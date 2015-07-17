test.examples <- function() {

Alice <- Player(name = "Alice", skill = Gaussian(mu = 25, sigma = 25 / 3))
Bob <- Player(name = "Bob", skill = Gaussian(mu = 25, sigma = 25 / 3))
Chris <- Player(name = "Chris", skill = Gaussian(mu = 25, sigma = 25 / 3))
Darren <- Player(name = "Darren", skill = Gaussian(mu = 25, sigma = 25 / 3))

Team1 <- Team(name = "1", rank = 1, list(Alice, Darren))
Team2 <- Team(name = "2", rank = 2, list(Bob, Chris))

teams <- list(Team1, Team2)

epsilon <- DrawMargin(draw_probability = 0.1, beta = 25 / 6, num_teams = length(teams))
parameters <- Parameters(beta = 25/6, epsilon, 25 / 300)

new_teams <- Trueskill(teams, parameters)

Print(new_teams)


}

test.deactivation <- function() {
  DEACTIVATED('Deactivating this test function')
}
