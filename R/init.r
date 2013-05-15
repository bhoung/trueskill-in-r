#' Compute the draw probability given the draw margin (epsilon)
#' Can use draw probability to estimate draw margin
#' or vice versa
#' @param epsilon how common draws area
#' @param beta randomness in game
#' @param total_players number of players 
DrawProbability <- function(epsilon = 0.10, beta = 1.0, total_players = 2) {
  return(2 * pnorm(epsilon / sqrt(total_players)) * beta - 1)
}

#' Compute EPSILON or draw margin, a measure of how likely a draw is.
#' takes draw probability as input
#' Can use draw probability to estimate draw margin
#' or vice versa
#' @param draw_probability draw probability
#' @param beta randomness in game
#' @param total_players number of players
DrawMargin <- function(draw_probability = 0.5, beta = 1.0, total_players = 2) {
  return(qnorm((draw_probability + 1.0) / 2) * sqrt(total_players) * beta)
}

INITIAL_MU = 25.0
INITIAL_SIGMA = INITIAL_MU / 3.0
INITIAL_BETA = INITIAL_SIGMA / 2.0                   
DRAW_PROBABILITY = 0.10
INITIAL_GAMMA = INITIAL_SIGMA / 100.0

#'  Sets three global parameters used in the TrueSkill algorithm.
#'
#'  @param beta is a measure of how random the game is.  You can think of it as
#'  the difference in skill (mean) needed for the better player to have
#'  an ~80% chance of winning.  A high value means the game is more
#'  random (I need to be *much* better than you to consistently overcome
#'  the randomness of the game and beat you 80% of the time); a low
#'  value is less random (a slight edge in skill is enough to win
#'  consistently).  The default value of beta is half of INITIAL_SIGMA
#'  (the value suggested by the Herbrich et al. paper).
#'
#'  @param epsilon is a measure of how common draws are.  Instead of specifying
#'  epsilon directly you can pass draw_probability instead (a number
#'  from 0 to 1, saying what fraction of games end in draws), and
#'  epsilon will be determined from that.  The default epsilon
#'  corresponds to a draw probability of 0.1 (10%).  (You should pass a
#'  value for either epsilon or draw_probability, not both.)
#'
#'  @param gamma is a small amount by which a player's uncertainty (sigma) is
#'  increased prior to the start of each game.  This allows us to
#'  account for skills that vary over time; the effect of old games
#'  on the estimate will slowly disappear unless reinforced by evidence
#'  from new games.
SetParameters = function(beta = INITIAL_BETA, draw_probability = DRAW_PROBABILITY, gamma = INITIAL_GAMMA) {
  BETA    <<- beta
  EPSILON <<- DrawMargin(draw_probability, BETA)
  GAMMA   <<- gamma
}



