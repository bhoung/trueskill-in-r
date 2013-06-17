DrawProbability <- function(epsilon = 0.7404666, beta = 25 / 6, num_teams = 2) {
  return(2 * pnorm(epsilon / sqrt(num_teams)) * beta - 1)
}

DrawMargin <- function(draw_probability = 0.10, beta = 25 / 6, num_teams = 2) {
  return(qnorm((draw_probability + 1.0) / 2) * sqrt(num_teams) * beta)
}

# default inputs are
# INITIAL_MU = 25.0
# INITIAL_SIGMA = INITIAL_MU / 3.0
# INITIAL_BETA = INITIAL_SIGMA / 2.0                   
# INITIAL_GAMMA = INITIAL_SIGMA / 100.0
# DRAW_PROBABILITY = 0.10
# INITIAL_EPSILON = DrawMargin(DRAW_PROBABILITY, BETA)

Parameters <- setRefClass('Parameters',
  fields = list(beta = "numeric", epsilon = "numeric", gamma = "numeric"),
  methods = list(
    initialize = function(beta = 25 / 6, epsilon = DrawMargin(draw_probability = 0.1, beta = 25 / 6), gamma = 25 / 300) {
      .self$beta <- beta
      .self$epsilon <- epsilon 
      .self$gamma <- gamma
      show()
    },
    show = function() {
      print(sprintf("Parameters [(beta, epsilon, gamma)]: [(%s, %s, %s)]", 
        round(beta, 3), round(epsilon, 3), round(gamma, 3)))
    } 
  )                                                                 
)
