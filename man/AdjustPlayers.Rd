\name{AdjustPlayers}
\alias{AdjustPlayers}
\title{Adjust the skills of a list of players.}
\usage{
  AdjustPlayers(players)
}
\arguments{
  \item{players}{is a list of player objects, for all the
  players who participated in a single game.  A 'player
  object' is any object with a 'skill' attribute (a
  Gaussian) and a 'rank' field. Lower ranks are better; the
  lowest rank is the overall winner of the game.  Equal
  ranks mean that the two players drew.

  This function updates all the 'skill' attributes of the
  player objects to reflect the outcome of the game.  The
  input list is not altered. Create all the variable nodes
  in the graph.  "Teams" are each a single player; there's
  a one-to-one correspondence between players and teams.
  (It would be straightforward to make multiplayer teams,
  but it's not needed for my current purposes.)}
}
\description{
  Adjust the skills of a list of players.
}

