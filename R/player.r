#' Player: class to hold the rank, skill and names of players
#' @param rank rank of player in the match outcome
#' @param skill skill of player represented by Gaussian object e.g. Gaussian(mu = 25, sigma = 25/3)
#' @param name name the player for display purposes
#' @param ... Reference Class inheritance
Player <- setRefClass("Player",
  fields = list(name = "character", skill = "Gaussian"),
  methods = list(
    initialize = function(name = "player", skill = Gaussian$new()) {
      .self$name <- name
      .self$skill <- skill
    }, 
    UpdateSkill = function(mu, sigma) {
      .self$skill <- Gaussian$new(mu = mu, sigma = sigma)
    },
    show = function() {
      print(sprintf("[player, skill]: [%s, [(%s, %s), (%s, %s)]]", 
        name, round(skill$MuSigma()[1], 3), round(skill$MuSigma()[2], 3), round(skill$pi, 3), round(skill$tau, 3)))
    }    
  )
)
