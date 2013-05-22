\name{Trueskill}
\alias{Trueskill}
\title{Apply Trueskill to Tournament Data}
\description{
    Trueskill function to be applied to tournament data in dataframe format.
  
    Data is required to be in long format with two rows for 
    each match, one with player 1 first and one with player 2 
    first. 
    
    Matches should be sorted such that the second copy
    of the match appears in the second half of the dataframe. 
    
    The package currently only supports the trueskill 
    algorithm with one player per team. Should this not match  
    the data you are interested, a function could be written from  
    AdjustPlayers, Player and Gaussian, and SetParameters.
    
    An example is provided at \url{https://gist.github.com/bhoung/5596282}
    (runtime is approx 50 secs).
}
\usage{
  Trueskill(data, parameters)
}  
\arguments{
  \item{data}{a data frame (\code{\link{data}}) with columns: Player, Opponent, margin.} 
  \item{parameters}{\code{\link{Parameters}}  object to hold input variables.}
}
