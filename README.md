trueskill: An implementation of the Trueskill algorithm in R
============================================================

Package: trueskill <br>
Title: An implementation of the TrueSkill algorithm in R <br>

Description:  

    An R implementation of the TrueSkill Algorithm (Herbrich, R., Minka, T. and Grapel, T), 
    a Bayesian skill rating system with inference by approximate message passing on a factor graph. 
    Used by Xbox to rank gamers and identify appropriate matches.
  
    http://research.microsoft.com/en-us/projects/trueskill/default.aspx 
    
    Requires R version 3.0 as it is written with Reference Classes.
  
    URL: https://github.com/bhoung/trueskill
    
    Acknowledgements to Doug Zongker and Heungsub Lee for their python implementations 
    of the algorithm and for the liberal reuse of Doug's code comments (@dougz and @sublee on github).

    Html documentation is at http://www.bhoung.com/trueskill/
    
    Example:
    
    Team1 <- Team("T1", rank = 1, list(Player("Alice", Gaussian(25, 8.3))))
    Team2 <- Team("T1", rank = 2, list(Player("Bob", Gaussian(25, 8.3))))
    teams <- list(Team1, Team2)
    
    epsilon <- DrawMargin(draw_probability = 0.1, beta = 25 / 6, num_teams = 2)
    parameters <- Parameters(beta = 25/6, epsilon, 25 / 300)
    
    teams <- Trueskill(teams, parameters)
    
Version: 0.2 <br>
Author: Brendan Houng <brendan.houng@gmail.com> <br>
License: GPL-3 <br>
Depends: R (>= 3.0)
