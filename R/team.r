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
