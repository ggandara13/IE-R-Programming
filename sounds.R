play.it <- function(music){
  play(music)
  Sys.sleep(1.6)
}

current.dir <-getwd()
setwd("/Users/gandara/documents/IE Master 2020/0 - R-Programming/Assigment 1/sounds")

gameover <- load.wave("gameover_astro.wav")
youfailed <- load.wave("youfailed.wav")
winner.voz <- load.wave("winner-robot.wav")
enjoy.playing <- load.wave("Mario-Enjoy Playing With You.wav")
you.won <- load.wave("Mario-Hey You Won.wav")
you.won2 <- load.wave("Mario-You Still Want To Play.wav")
i.won <- load.wave("Mario-I Won.wav")
lets.play <- load.wave("Mario-Let's Play.wav")
try.again <- load.wave("Mario-Try Again.wav")
you.first.goodluck <- load.wave("Mario-You Go First Good Luck.wav")
you.first <- load.wave("Mario-You Go First.wav")
still.play <- load.wave("Mario-You Still Want To Play.wav")
another.game <- load.wave("Mario-Another Game.wav")

setwd(current.dir)
