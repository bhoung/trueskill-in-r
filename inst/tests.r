library('RUnit')
 
#dropbox = "C:/Dropbox/"
dropbox = "/Users/brendanhoung/Dropbox/"
source(paste(dropbox, "trueskill-in-r/R/trueskill.r", sep = ""))
source(paste(dropbox, "trueskill-in-r/R/factorgraph.r", sep = ""))
source(paste(dropbox, "trueskill-in-r/R/player.r", sep = ""))
source(paste(dropbox, "trueskill-in-r/R/team.r", sep = ""))
source(paste(dropbox, "trueskill-in-r/R/init.r", sep = ""))

test.suite <- defineTestSuite("trueskill",
  dirs = file.path(paste(dropbox, "trueskill-in-r/tests", sep = "")),
  testFileRegexp = '^\\d+\\.r')
 
test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)
