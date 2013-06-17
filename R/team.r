Team <- setRefClass("Team",
  fields = list(rank = "numeric", players = "list", name = "character"),
  methods = list(
    initialize = function(name = "team", rank = 1, players = list(Player(name = "player", skill = Gaussian(25, 25 / 3)))) {
      .self$players <- players
      .self$rank <- rank
      .self$name <- name
    },
    # Map of list and print?
    show = function() {
      print(sprintf("[team, rank]: [%s, %s]",  name, rank))
      print(players)   
    }    
  )
)

GetPlayers <- function(teams) {
  GetPlayer <- function(team) { return(team$players) }
  return(unlist(Map(GetPlayer, teams))) 
}

Trueskill = function(teams, parameters = Parameters()) {
  
  # dependencies on parameters:
  # prior factor, gamma
  # likelihood factor, beta
  # truncate factor, epsilon
  
  SortRank = function(teams) {
    GetRank = function(x) return(x$rank)
    sorted_teams <- teams[order(unlist(Map(GetRank, teams)))] 
    return(sorted_teams)
  }
  
  teams <- SortRank(teams)
  
  # skill, performance and team variable, as well as the difference variable
  # create var from player object, then map over list of players
  
  GenVar <- function(var, varname) {
    return(Variable(name = paste(varname, var$name, sep = "_")))
  }
  
  GenSumFactor <- function(team_var, perf_vars) {
    num_players <- length(perf_vars)
    coeff <- rep(list(1), num_players)
    return(SumFactor(sum_variable = team_var, term_variables = perf_vars, coeff = coeff, name = paste("SF", team_var$name, sep = "_")))
  }                                                                                    
  
  # Team Diff SumFactor
  GenTeamDiff <- function(diff_var, match_list) {
    match_name <- paste("SF", match_list[[1]]$name, "vs.", match_list[[2]]$name, sep = " ")
    return(SumFactor(sum_variable = diff_var, term_variables = match_list, coeff = list(1, -1), name = match_name))
  }
  
  # zip teams less last team, with teams less first team (t1, t2, t3) with (t2, t3, t4)
  GenTeamDiffList <- function(diff_vars, team_vars) {
    match_list <- mapply(list, team_vars[-length(team_vars)], team_vars[-1], SIMPLIFY = F)
    return(mapply(GenTeamDiff, diff_vars, match_list, SIMPLIFY = F))
  }
  
  GenTruncateFactor <- function(diff_var, player1, player2, epsilon) {
    
    if(player1$rank == player2$rank) { V <- Vdraw; W <- Wdraw }
    else { V <- Vwin ; W <- Wwin }
    
    return(TruncateFactor(diff_var, V, W, epsilon, name = paste("TF", player1$name, player2$name, sep = "_")))    
  }
  

  # create skill and perf vars for each player in each team
  # then skill and skill to perf factors
  GenSkillVars <- function(team) { 
    GenSkill <- function(player, varname) {
      return(Variable(value = player$skill, name = paste(varname, player$name, sep = "_")))
    }
    return(mapply(GenSkill, team$players, "skill")) 
  }
  
  GenPerfVars <- function(team) { return(mapply(GenVar, team$players, "perf")) }
  
  # Create each layer of factor nodes.  At the top we have priors
  # initialized to the player's current skill estimate.
  
  GenPriorFactor <- function(skill_var, player, gamma) {
    new_sigma <- sqrt(player$skill$sigma() ^ 2 + gamma ^ 2)
    param <- Gaussian(mu = player$skill$mu(), sigma = new_sigma)
    return(PriorFactor(variable = skill_var, param = param, name = paste("PF", player$name, sep = "_")))
  }
  
  GenLikelihoodFactor <- function(skill_var, perf_var, player, beta) {
    return(LikelihoodFactor(skill_var, perf_var, beta ^ 2, name = paste("LF", player$name, sep = "_")))
  }                                                               
  
  players <- GetPlayers(teams)
  
  skill_vars <- unlist(mapply(GenSkillVars, teams))
  team_perf_vars <- mapply(GenPerfVars, teams)
  
  # need to pass nested list of perf vars to GenSumFactor
  if(length(teams) == length(players)) { team_perf_vars <- Map(list, team_perf_vars) }
  perf_vars <- unlist(team_perf_vars)
  
  # create team vars and diff vars for each team 
  team_vars <- mapply(GenVar, teams, "team")
                                                                           
  team_names <- GetNames(teams)
  match_list <- mapply(list, team_names[-length(team_names)], team_names[-1], SIMPLIFY = F)
  
  GenDiffVar <- function(match_list, varname) {
    match_name <- paste(match_list[[1]], "vs.", match_list[[2]], sep = " ")
    return(Variable(name = paste(varname, match_name, sep = "_")))
  }
  
  diff_vars <- mapply(GenDiffVar, match_list, "diff")

  skill <- mapply(GenPriorFactor, skill_vars, players, parameters$gamma)
  skill_to_perf <- mapply(GenLikelihoodFactor, skill_vars, perf_vars, players, parameters$beta)

  perf_to_team <- mapply(GenSumFactor, team_vars, team_perf_vars)
  team_diff <- GenTeamDiffList(diff_vars, team_vars)
  
  # At the bottom we connect adjacent teams with a 'win' or 'draw'
  # factor, as determined by the rank values.
  
  trunc <- mapply(GenTruncateFactor, diff_vars, teams[-length(teams)], teams[-1], parameters$epsilon)
  
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

  if (length(teams) == 2) {
  		  
    z <- 1
    while (z <= 10) {
      team_diff[[1]]$UpdateSum()   # arrows (1) and (4)
      trunc[[1]]$Update()          # arrows (2) and (5)
      team_diff[[1]]$UpdateTerm(1) # arrow (3) 
      team_diff[[1]]$UpdateTerm(2) # arrow (6)
      z <- z + 1
    }          
  } else {
    z <- 1
    while (z <= 20) {
    		
      N <- length(team_diff)   
      M <- N - 1
      
      # up and right
      for (i in 1:M) {
      	team_diff[[i]]$UpdateSum()   # arrows (1)
      	trunc[[i]]$Update()          # arrows (2)
      	team_diff[[i]]$UpdateTerm(2) # arrows (3)
      }
      
      # up and left
      for (j in N:2) {
         team_diff[[j]]$UpdateSum()   # arrows (4)
         trunc[[j]]$Update()          # arrows (5)
         team_diff[[j]]$UpdateTerm(1) # arrows (6)
      }
      z <- z + 1
    }
    
    team_diff[[1]]$UpdateTerm(1)
    team_diff[[N]]$UpdateTerm(2)
    
  }
  
  # Now we push messages back up the graph, from the teams back to the
  # player skills.
  
  UpdateTerms <- function(sumfactor) {
    for (i in 1:length(sumfactor$terms)) { 
      sumfactor$UpdateTerm(i) 
    } 
  }
  # number of calls to UpdateTerm depends on number of players per team
  Map(UpdateTerms, perf_to_team)
  Map(function(x) x$UpdateMean(), skill_to_perf)
  
  # Finally, the players' new skills are the new values of the s
  # variables.
  UpdateSkill <- function(player, skill_var) {
    player$UpdateSkill(skill_var$value$mu(), skill_var$value$sigma())     
  }
  
  mapply(UpdateSkill, players, skill_vars, SIMPLIFY = F)
  
  #' display list of players nicer
  #' @param list a list of player objects
  
  return(teams)
}


PrintPlayers = function(list) {
  for(i in 1:length(list)) {
    print(list[[i]]$players)
  }
}

PrintList = function(list) {
  for(i in 1:length(list)) {
    print(list[[i]])
  }
}               
