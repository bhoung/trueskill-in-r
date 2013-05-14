# Copyright 2013 Brendan Houng
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#
# An R implementation of the TrueSkill algorithm. 
# Written using object oriented reference classes because it is easier to create and update the factor graph 
# using pass by reference programming rather than pass by value. This is the default used by R where 
# a new copy of every object is made with each operation.
# https://github.com/bhoung/trueskill-in-r

# Acknowledgements to Doug Zongker and Heungsub Lee for their python implementations of the algorithm
# Respectively: https://github.com/dougz/trueskill & https://github.com/sublee/trueskill
# 
# References:
# 1. TrueSkill: A Bayesian Skill Rating System
# 2. Jeff Moser's explanatory notes
# 3. Hadley Wickham's book on R programming and package developing (work in progress)
# 
# Microsoft has an online calculator to compare against. 
# http://research.microsoft.com/en-us/projects/trueskill/
# * I noticed that sometimes skill levels are attributed to the wrong player in that calc.


# Compute the draw probability given the draw margin (epsilon)
DrawProbability <- function(epsilon = 0.10, beta = 1.0, total_players = 2) {
  return(2 * pnorm(epsilon / sqrt(total_players)) * beta - 1)
}

DrawMargin <- function(p = 0.5, beta = 1.0, total_players = 2) {
  return(qnorm((p + 1.0) / 2) * sqrt(total_players) * beta)
}

#  Sets three global parameters used in the TrueSkill algorithm.
#
#  beta is a measure of how random the game is.  You can think of it as
#  the difference in skill (mean) needed for the better player to have
#  an ~80% chance of winning.  A high value means the game is more
#  random (I need to be *much* better than you to consistently overcome
#  the randomness of the game and beat you 80% of the time); a low
#  value is less random (a slight edge in skill is enough to win
#  consistently).  The default value of beta is half of INITIAL_SIGMA
#  (the value suggested by the Herbrich et al. paper).
#
#  epsilon is a measure of how common draws are.  Instead of specifying
#  epsilon directly you can pass draw_probability instead (a number
#  from 0 to 1, saying what fraction of games end in draws), and
#  epsilon will be determined from that.  The default epsilon
#  corresponds to a draw probability of 0.1 (10%).  (You should pass a
#  value for either epsilon or draw_probability, not both.)
#
#  gamma is a small amount by which a player's uncertainty (sigma) is
#  increased prior to the start of each game.  This allows us to
#  account for skills that vary over time; the effect of old games
#  on the estimate will slowly disappear unless reinforced by evidence
#  from new games.

INITIAL_MU = 25.0
INITIAL_SIGMA = INITIAL_MU / 3.0
INITIAL_BETA = INITIAL_SIGMA / 2.0                   
DRAW_PROBABILITY = 0.10
INITIAL_GAMMA = INITIAL_SIGMA / 100.0

# epsilon is a measure of how often draws occur
# can use draw probability (e.g. 10%) to set EPSILON

# alternatively set INITIAL_EPSILON as DrawProbability()
# INITIAL_EPSILON = DrawProbability()

SetParameters = function(beta = INITIAL_BETA, draw_probability = DRAW_PROBABILITY, gamma = INITIAL_GAMMA) {
  BETA    <<- beta
  EPSILON <<- DrawMargin(draw_probability, BETA)
  GAMMA   <<- gamma
}

SetParameters()

# normal with mean mu, and standard deviation sigma
# precision is pi, 1 / sigma squared
# tau is mu * pi, the precision adjusted mean

# multiply Gaussians
# update mu and sigma
Multiply <- function(x, y) {
    z <- Gaussian$new()
    z$pi  <- x$pi + y$pi
    z$tau <- x$tau + y$tau
    z
}

# divide Gaussians
# update mu and sigma
Divide <- function(x, y) {
    z <- Gaussian$new()
    z$pi  <- x$pi - y$pi
    z$tau <- x$tau - y$tau
    z
}

Gaussian <- setRefClass('Gaussian',
  fields = list(pi = "numeric", tau = "numeric"),
  methods = list(
    initialize = function(mu = NULL, sigma = NULL, pi = NULL, tau = NULL) {
      if(!is.null(pi) & !is.null(tau)) {
        .self$pi <- pi
        .self$tau <- tau
      }
      else if(!is.null(mu) & !is.null(sigma)) {
        .self$pi <- sigma ^ -2
        .self$tau <- .self$pi * mu
      }
      else {
        .self$pi <- 0
        .self$tau <- 0
      }
      .self 
    },
    MuSigma = function() {
      if(pi == 0) {
        mu = 0
        sigma = Inf
      }
      else {
        mu = tau / pi
        sigma = sqrt(1 / pi)
      }
      return(c(mu, sigma))
    },
    show = function() {
      if(pi == 0) {
        mu = 0
        sigma = Inf
      }
      else {
        mu = tau / pi
        sigma = sqrt(1 / pi)
      }	    
      print(sprintf("Guassian [(mu, sigma), (pi, tau)]: [(%s, %s), (%s, %s)]", 
        round(mu, 3), round(sigma, 3), round(pi, 3), round(tau, 3)))
    }, 
    mu = function() {
      if(pi == 0) {
        mu = 0
      }
      else {
        mu = tau / pi
      }
      return(mu)
    },
    sigma = function() {
      if(pi == 0) {
        sigma = Inf
      }
      else {
        sigma = sqrt(1 / pi)
      }
      return(sigma)
    }
  )                                                                 
)

setMethod("*", signature(e1 = "Gaussian", e2 = "Gaussian"), function(e1, e2) Multiply(e1, e2))
setMethod("/", signature(e1 = "Gaussian", e2 = "Gaussian"), function(e1, e2) Divide(e1, e2))


GetName <- function(x) { return(x$name) }
GetNames <- function(list) { return(Map(GetName, list)) }

PrettyPrint <- function(x) { 
  string <- c("[\"")
  string <- cbind(string, list(x[1]))
  if (length(x) > 1) {
    for(i in 2:length(x)) {
      string <- cbind(string, c("\", \""))
      string <- cbind(string, list(x[i]))
    }
  }
  string <- cbind(string, c("\"]"))
  return(string)
}


# ... and callSuper is for potential classes that inhererit from Gaussian
# http://stat.ethz.ch/R-manual/R-devel/library/methods/html/refClass.html

# "messages from factors", called factors in Doug Zongker's version 
# it stores messages in Gaussian form
# and are referenced by factor name
Variable <- setRefClass("Variable", 
  fields = list(value = "Gaussian", name = "character", messages = "list"),
  methods = list(
    initialize = function(value = Gaussian$new(), name = "", messages = list(), ...) {
      .self$name <- name
      .self$value <- value
      .self$messages <- messages
      callSuper(...)
      .self
    },
    # update message to be new message
    # save old message
    # update value to be (value / old message) * new message
    UpdateMessage = function(factor = Factor$new(), message = Gaussian$new()) {
      old_message <- messages[[factor$name]]
      value <<- value / old_message * message
      messages[factor$name] <<- message
      .self
    },  
    # .self$variable cannot have assignment <<- but must have <-
    UpdateValue = function(factor = Factor$new(), new_value = Gaussian$new()) {
      old_message <- messages[[factor$name]]
      old_value <- value
      messages[factor$name] <<- (new_value * old_message) / old_value
      .self$value <- new_value
      .self
    },  
    GetMessage = function(factor) {
      messages[[factor$name]]
    },
    show = function() {
      mu <- value$tau / value$pi
      sigma <- sqrt(1 / value$pi)
      print(sprintf("[Variable] %s with value [(mu, sigma), (pi, tau)]: [(%s, %s), (%s, %s)]",
        name, round(mu, 3), round(sigma, 3), round(value$pi, 3), round(value$tau, 3)))
      if (length(messages) == 0) {
         print("Messages from [Factors] have not been assigned")
      }
      else {
        # factors is not a list of factors, but a list of Gaussians with factor names as keys 
        factor_names <- names(messages)
        string <- PrettyPrint(factor_names)
        string <- cbind("[Factors]: ", string)
        do.call("cat", string)
      }
    }
  ) 
)

g1 <- Gaussian$new()
v <- Variable$new(g1, name = "v")
v

Factor <- setRefClass("Factor",
  fields = list(variables = "list", name = "character"),
  methods = list(
    initialize = function(variables = list(), name = "", ...) {
      .self$name <- name
      .self$variables <- variables
      if(length(variables) > 0) {
        for (i in 1:length(variables)) {
           AttachFactor(variables[[i]])
        }                  
      }
      .self
    },
    AttachFactor = function(variable) {
      variable$messages[name] <- Gaussian$new()
    },
    show = function() {
      variable_names <- unlist(GetNames(variables))
      string <- cbind("[Factor]:", name, "with [Variable(s)]:",  PrettyPrint(variable_names))
      do.call("cat", string)
    }
  )
)


# standard normal with mean 0 and std 1
g1 <- Gaussian$new(2, 1)
g2 <- Gaussian$new(1.5, 1)

g1 * g2                     
Multiply(g1, g2)

v1 <- Variable$new(g1, "v1")
v2 <- Variable$new(g2, "v2")

f1 <- Factor$new(list(v1, v2), "f1")
f2 <- Factor$new(list(v1, v2), "f2")

v2
f1

message <- Gaussian$new(1.2, 1.5)
v2$UpdateMessage(f1, message)
value <- Gaussian$new(1.2, 2)
value
v2$UpdateValue(f1, value)                      
v2$GetMessage(f1)

# Factor classes that implement the 5 update equationsin Herbrich
# 1. PriorFactor - That is, the starting skill of a player, push the parameters to the variable.

PriorFactor <- setRefClass("PriorFactor",
  contains = "Factor",
  fields = list(variable = "Variable", param = "Gaussian"),
  methods = list(
    initialize = function(variable = Variable$new(), param = Gaussian$new(), ...) {
      variables <- list(variable)
      callSuper(variables, ...)
      .self$param <- param
    },
    Start = function() {
      variables[[1]]$UpdateValue(.self, param)
    },
    show = function() {
      variable_names <- unlist(GetNames(variables))
      string <- cbind("[PriorFactor]:", name, "with [Variable]:",  PrettyPrint(variable_names))
      do.call("cat", string)
    }
  )
)

pf2 <- PriorFactor$new(variable = v1, param = g1, name = "pf2")

pf2$Start()
                       
# Connects two variables, the value of one being the mean of the message sent to the other.
LikelihoodFactor <- setRefClass("LikelihoodFactor",
  contains = "Factor",
  fields = list(mean = "Variable", value = "Variable", variance = "numeric"),
  methods = list(
    initialize = function(mean_variable = Variable$new(), value_variable = Variable$new(), variance = BETA^2, ...) {
      callSuper(variables = list(mean_variable, value_variable), ...)
      .self$mean = mean_variable
      .self$value = value_variable
      .self$variance = variance
    },
    UpdateValue = function() {
    "Update the value after a change in the mean (going down) in the TrueSkill factor graph."
      y = (.self$mean)$value
      fy = (.self$mean)$GetMessage(.self)
      a = 1.0 / (1.0 + .self$variance * (y$pi - fy$pi))
      message <- Gaussian$new(pi = a * (y$pi - fy$pi), tau = a * (y$tau - fy$tau))
      # print(sprintf("message from [Likelihood Factor]: %s", .self$name))
      # print(message)
      value$UpdateMessage(.self, message)
    },
    UpdateMean = function() {
    "Update the mean after a change in the value (going up in the TrueSkill factor graph. "
      # Note this is the same as UpdateValue, with self.mean and self.value interchanged.
      x = (.self$value)$value
      fx = (.self$value)$GetMessage(.self)
      a = 1.0 / (1.0 + .self$variance * (x$pi - fx$pi))
      mean$UpdateMessage(.self, Gaussian$new(pi = a * (x$pi - fx$pi), tau = a * (x$tau - fx$tau)))
    }
  )
)

lf1 <- LikelihoodFactor$new(name = "LF1")
(lf1$value)$value
(lf1$value)$GetMessage(lf1)
(lf1$mean)$UpdateMessage(lf1, Gaussian$new())

# SumFactor$UpdateTerm()
# Swap the coefficients around to make the term we want to update
# be the 'sum' of the other terms and the factor's sum, eg.,
# change:
#
#    x = y_1 + y_2 + y_3
#
# to
#
#    y_2 = x - y_1 - y_3
#
# then use the same update equation as for UpdateSum.
      
SumFactor <- setRefClass("SumFactor",
  # A factor that connects a sum variable with 1 or more terms, which are summed after being multiplied by fixed (real) coefficients.
  contains = "Factor",
  fields = list(sum = "Variable", terms = "list", coeffs = "list"),
  methods = list(
    initialize = function(sum_variable = Variable$new(), term_variables = list(Variable$new()), coeffs = list(1), ...) {
      stopifnot(length(coeffs) == length(term_variables))
      .self$sum = sum_variable
      .self$terms = term_variables
      .self$coeffs = coeffs
      callSuper(variables = append(list(sum_variable), term_variables), ...)
    },
    # var is a Variable, y is a list of Values, fy is a list of messages, a is a list of numerics
    InternalUpdate = function(var = Variable$new(), y = list(), fy = list(), a = list()) {
      fn = function(a, y, fy) return(a^2 / (y$pi - fy$pi))
      fn2 = function(a, y, fy) return(a * (y$tau - fy$tau) / (y$pi - fy$pi))
      
      new_pi = 1.0 / sum(unlist(mapply(fn, a, y ,fy, SIMPLIFY = F)))
      new_tau <- new_pi * sum(unlist(mapply(fn2, a, y, fy, SIMPLIFY = F)))
      
      new_msg <- Gaussian(pi = new_pi, tau = new_tau)
      #print(new_msg)
      var$UpdateMessage(.self, new_msg)
    },
    UpdateSum = function() {
      "Update the sum value (down in the factor graph)."
      y <- Map(function(x) return(x$value), terms)  	    
      fy <- Map(function(x) return(x$GetMessage(.self)), terms)
      a <- coeffs
      InternalUpdate(sum, y, fy, a)
    },
    UpdateTerm = function(index) {
      " Update one of the term values (up in the factor graph). "
      b <- coeffs
      a <- list()
      for(i in 1:length(b)) {
      	a[[i]] <- -b[[i]] / b[[index]]
      }
      
      a[index] <- 1 / b[[index]]
      
      v <- .self$terms
      v[index] <- .self$sum
      
      y <- Map(function(x) return(x$value), v)
      fy <- Map(function(x) return(x$GetMessage(.self)), v)
      
      InternalUpdate(terms[[index]], y , fy, a)
    }                                                    
  )
)

sf1 <- SumFactor$new(name = "SF1")
# utils::str(sf1)

# update rules for approximate marginals for the win and draw cases
# required by truncate factor
# dnorm is PDF
# pnorm is CDF
# qnorm is inverse CDF

           
# what is e and t

Vwin <- function(t, e) {
  x <- t - e
  return(dnorm(x) / pnorm(x))
}

Wwin <- function(t, e) {
  return(Vwin(t, e) * (Vwin(t, e) + t - e))
}

Vdraw <- function(t, e) {
  return((dnorm(-e - t) - dnorm(e - t)) / (pnorm(e - t) - pnorm(-e - t)))
}

Wdraw <- function(t, e) {
  return((Vdraw(t, e) ^ 2) + ((e - t) * dnorm(e - t) + (e + t) * dnorm(e + t)) / (pnorm(e - t) - pnorm( -e - t)))		
}

Vwin(1, 2)
Wwin(1, 2)
Vdraw(1, 2)
Wdraw(1, 2)
dnorm(1, mean = 0 , sd = 1)

TruncateFactor <- setRefClass("TruncateFactor",
  contains = "Factor",
  fields = list(variable = "Variable", V = "function", W = "function", epsilon = "numeric"),
  methods = list(
    initialize = function(variable = Variable$new(), V = Vwin, W = Wwin, epsilon = 0.10, ...) {
      callSuper(variables = list(variable), ...)
      .self$variable = variable
      .self$V = V
      .self$W = W
      .self$epsilon = epsilon
    },
    Update = function() {
      x = (.self$variable)$value
      fx = (.self$variable)$GetMessage(.self)
      
      div <- x / fx
      
      t <- div$tau / sqrt(div$pi)
      e <- .self$epsilon * sqrt(div$pi) 
      
      v <- V(t, e)
      w <- W(t, e)
 
      new_pi <- (div$pi / (1.0 - w))
      new_tau <- (div$tau + sqrt(div$pi) * v) / (1.0 - w)
      
      new_val <- Gaussian(pi = new_pi, tau = new_tau)
      variable$UpdateValue(.self, new_val)
    },
    show = function() {
      variable_names <- unlist(GetNames(variables))
      string <- cbind("[Truncate Factor]:", name, "with [Variable(s)]:", PrettyPrint(variable_names))
      do.call("cat", string)
    }
  )
)



Player <- setRefClass("Player",
  fields = list(skill = "Gaussian", rank = "numeric", name = "character"),
  methods = list(
    initialize = function(skill = Gaussian$new(), rank = 1, name = "") {
      .self$skill <- skill
      .self$rank <- rank
      .self$name <- name
    }, 
    UpdateSkill = function(mu, sigma) {
      .self$skill <- Gaussian$new(mu = mu, sigma = sigma)
    }
  )
)

AdjustPlayers = function(players) {
  #   "Adjust the skills of a list of players.
  # 
  #   'players' is a list of player objects, for all the players who
  #   participated in a single game.  A 'player object' is any object with
  #   a 'skill' attribute (a Gaussian) and a 'rank' field.
  #   Lower ranks are better; the lowest rank is the overall winner of the
  #   game.  Equal ranks mean that the two players drew.
  # 
  #   This function updates all the 'skill' attributes of the player
  #   objects to reflect the outcome of the game.  The input list is not
  #   altered."
  
  SortPlayers = function(players) {
    GetRank = function(x) return(x$rank)
    sorted_players <- players[order(unlist(Map(GetRank, players)))] 
    return(sorted_players)
  }
  
  players <- SortPlayers(players)

  # Create all the variable nodes in the graph.  "Teams" are each a
  # single player; there's a one-to-one correspondence between players
  # and teams.  (It would be straightforward to make multiplayer
  # teams, but it's not needed for my current purposes.)
  
  # skill, performance and team variable, as well as the difference variable

  # create var from player object, then map over list of players
  GenSkill <- function(player, varname) return(Variable$new(value = player$skill, name = paste(varname, player$name, sep = "")))
  GenVar <- function(player, varname) return(Variable$new(name = paste(varname, player$name, sep = "")))
                               
  # Create each layer of factor nodes.  At the top we have priors
  # initialized to the player's current skill estimate.
  
  GenPriorFactor <- function(skill_var, player) {
    new_sigma <- sqrt(player$skill$sigma()^ 2 + GAMMA ^ 2)
    param <- Gaussian$new(mu = player$skill$mu(), sigma = new_sigma)
    return(PriorFactor$new(variable = skill_var, param = param, name = paste("PF", player$name, sep = "")))
  }
  
  GenLikelihoodFactor <- function(skill_var, perf_var, player) {
    return(LikelihoodFactor$new(skill_var, perf_var, BETA ^ 2, name = paste("LF", player$name, sep = "")))
  }                                                               
  
  GenSumFactor <- function(team_var, perf_var, player) {
    return(SumFactor$new(sum_variable = team_var, term_variables = list(perf_var), coeff = list(1), name = paste("SF", player$name, sep = "")))
  }                                                                                    
  
  # Team Diff SumFactor
  GenTeamDiff <- function(diff_var, match_list) {
    match_name <- paste("SF", match_list[[1]]$name, "vs.", match_list[[2]]$name, sep = " ")
    return(SumFactor$new(sum_variable = diff_var, term_variables = match_list, coeff = list(1, -1), name = match_name))
  }
  
  # zip teams less last team, with teams less first team (t1, t2, t3) with (t2, t3, t4)
  GenTeamDiffList <- function(diff_vars, team_vars) {
    match_list <- mapply(list, team_vars[-length(team_vars)], team_vars[-1], SIMPLIFY = F)
    return(mapply(GenTeamDiff, diff_vars, match_list, SIMPLIFY = F))
  }

  GenTruncateFactor <- function(diff_var, player1, player2) {
    
    if(player1$rank == player2$rank) { V <- Vdraw; W <- Wdraw }
    else { V <- Vwin ; W <- Wwin }
    
    return(TruncateFactor$new(diff_var, V, W, EPSILON, name = paste("TF", player1$name, player2$name, sep = "_")))	  
  }
  
  skill_vars <- mapply(GenSkill, players, "skill")
  perf_vars <- mapply(GenVar, players, "perf") 
  team_vars <- mapply(GenVar, players, "team")         
  diff_vars <- mapply(GenVar, players[-length(players)], "diff")
                                               
  skill <- mapply(GenPriorFactor, skill_vars, players)
  skill_to_perf <- mapply(GenLikelihoodFactor, skill_vars, perf_vars, players)
  perf_to_team <- mapply(GenSumFactor, team_vars, perf_vars, players)
  team_diff <- GenTeamDiffList(diff_vars, team_vars)

  # At the bottom we connect adjacent teams with a 'win' or 'draw'
  # factor, as determined by the rank values.
  
  trunc <- mapply(GenTruncateFactor, diff_vars, players[-length(players)], players[-1])
  
  # Start evaluating the graph by pushing messages 'down' from the
  # priors.
  Map(function(x) x$Start(), skill)
  Map(function(x) x$UpdateValue(), skill_to_perf)
  Map(function(x) x$UpdateSum(), perf_to_team)
  
  # Because the truncation factors are approximate, we iterate,
  # adjusting the team performance (t) and team difference (d)
  # variables until they converge.  In practice this seems to happen
  # very quickly, so I just do a fixed number of iterations.
  #
  # This order of evaluation is given by the numbered arrows in Figure
  # 1 of the Herbrich paper.

  z <- 1
  while (z <= 10) {
    Map(function(x) x$UpdateSum(), team_diff)                        # arrows (1) and (4)
    Map(function(x) x$Update(), trunc)                               # arrows (2) and (5)
    Map(function(x) { x$UpdateTerm(1); x$UpdateTerm(2) }, team_diff) # arrows (3) and (6)
    z <- z + 1
  }
    		
  # Now we push messages back up the graph, from the teams back to the
  # player skills.

  Map(function(x) x$UpdateTerm(1), perf_to_team)
  Map(function(x) x$UpdateMean(), skill_to_perf)
  
  # Finally, the players' new skills are the new values of the s
  # variables.
                                 
  UpdateSkill <- function(player, skill_var) {
     player$UpdateSkill(skill_var$value$mu(), skill_var$value$sigma())     
  }
  
  mapply(UpdateSkill, players, skill_vars, SIMPLIFY = F)
  return(players)      
}
