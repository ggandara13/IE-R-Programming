#install.packages("audio")
library("audio")


setwd("/Users/gandara/Documents/IE Master 2020/0 - R-Programming/Assigment 1")
source("lcg.R")
source("sounds.R")

value.name <- "PLAYER"
initial.seed <- 1234
current.seed <- initial.seed

# This function force to get only numbers and based on the parameter a range between those 2
get.number.choice <- function(message.prompt, low.param, up.param){
  n <- up.param + 1
  while (!(n >= low.param && n<=up.param )){
    n <- readline(message.prompt)
    n <- ifelse(grepl("\\D",n),-1,as.integer(n))
    if(is.na(n)){break}  # breaks when hit enter or null
  }
  return(n)
}

print(paste("Welcome to Human Behavior Prediction"))
value.name <- readline(prompt="What is your name? ")


#Create the Main loop
first.greeting.time <- 1
keep.playing <- "Y"

while (keep.playing == "Y"){
  value.typed <- get.number.choice("Choose the type of game (1: Easy; 2: Difficult): ", 1, 2)
  # >2 or anything else would be consider 'difficult' - 2 options in the else only
  type.of.game <- as.integer(value.typed)
  
  value.typed <- get.number.choice("Enter the number of moves: ", 1, 999)
  number.of.moves <- as.integer(value.typed)
  
  #start game greeting
  if (first.greeting.time == 1)  {
    play.it(lets.play)
    play.it(you.first.goodluck)
  } else {
    play.it(you.first)
  }
  
  # EASY
  if(type.of.game == 1) { # Easy
    computer.numb.victories <- 0
    player.numb.victories <- 0
  
    for (j in 1:number.of.moves){
      print("---")
  
      # The computer move has to be random
      x <- linear.congruence(seed=current.seed)
      computer.move <- x[1]
      new.seed <- x[2]
      current.seed <- new.seed
      
      value.typed <- get.number.choice(paste("Choose your move number ", as.character(j), " (0 or 1): "), 0, 1)
      human.move <- as.integer(value.typed)
      
      if (human.move == computer.move){ # Computer correctly guessed!
        print(paste("player =",human.move," machine =",computer.move," - Computer wins!"))
        computer.numb.victories <- computer.numb.victories +1
      } else {
        print(paste("player =",human.move," machine =",computer.move," - Player wins!"))
        player.numb.victories <- player.numb.victories +1
      }
    
      message2print <- "PLAYER: "
      if(player.numb.victories>0) {
        for (i in 1:player.numb.victories) {
          message2print <- paste(message2print,"*",sep="")
        }
      }
      print(message2print)
    
  
      message2print <- "COMPUTER: "
      if(computer.numb.victories>0) {
        for (i in 1:computer.numb.victories) {
          message2print <- paste(message2print,"*",sep="")
        }
      }
      print(message2print)
    }
  
    if(player.numb.victories < computer.numb.victories) {
      print(paste("Easy game is over, final score: ",  value.name,
                player.numb.victories, 
                "-", 
                computer.numb.victories,"computer – The COMPUTER won!"))
      play.it(i.won)
    } else if (player.numb.victories > computer.numb.victories) {
        print(paste("Easy game is over, final score: ",  value.name,
                player.numb.victories, 
                "-", 
                computer.numb.victories,"computer – You won!"))
        play.it(you.won)
    } else {
      print(paste("Easy game is over, final score: ",  value.name,
                player.numb.victories, 
                "-", 
                computer.numb.victories,"It was a tie!"))
      play.it(try.again)
      print("Play again against the computer and see if you are able to beat it!")
    }
  } 
  else { #DIFFICULT
    # to start will be 0, next time has to be randomly chosen
  
    throw00 = 0
    throw01 = 0
    throw10 = 0
    throw11 = 0
  
    computer.numb.victories <- 0
    player.numb.victories <- 0
  
    #random chosen the first computer move
    x <- linear.congruence(seed=current.seed)
    computer.move <- x[1]
    new.seed <- x[2]
    current.seed <- new.seed
    
    last.move <- 0
  
    for (j in 1:number.of.moves){
      print("---")
    
      #------ This area is to choose the computer-move
    
      if (last.move ==0) {
        if (throw10 > throw00) {
          computer.move <- 1
        } else if (throw10 < throw00) {
          computer.move <- 0
        } else {
          # will be selected random 0,1
          x <- linear.congruence(seed=current.seed)
          computer.move <- x[1]
          new.seed <- x[2]
          current.seed <- new.seed
        }
      } else {
        if (throw11 > throw01) {
          computer.move <- 1
        } else if (throw11 < throw01) {
          computer.move <- 0
        } else {
          # will be selected random 0,1
          x <- linear.congruence(seed=current.seed)
          computer.move <- x[1]
          new.seed <- x[2]
          current.seed <- new.seed
        }
      }
    
      #------ Finish the area to choose the computer-move
      
      # player moves  
      value.typed <- get.number.choice(paste("Choose your move number ", as.character(j), " (0 or 1): "), 0 , 1)
      human.move <- as.integer(value.typed)
    
      #Increment the player conditional moves based on the choice
      if (human.move ==0 & last.move == 1) throw01 <- throw10 +1  
      if (human.move ==1 & last.move == 1) throw11 <- throw11 +1  
      if (human.move ==0 & last.move == 0) throw00 <- throw00 +1  
      if (human.move ==1 & last.move == 0) throw10 <- throw10 +1  
    
      last.move <- human.move
    
      if (human.move == computer.move){ # Computer correctly guessed!
        print(paste("player =",human.move," machine =",computer.move," - Computer wins!"))
        computer.numb.victories <- computer.numb.victories +1
      } else {
        print(paste("player =",human.move," machine =",computer.move," - Player wins!"))
        player.numb.victories <- player.numb.victories +1
      }
    
      message2print <- "PLAYER: "
      if(player.numb.victories>0) {
       for (i in 1:player.numb.victories) {
          message2print <- paste(message2print,"*",sep="")
        }
      }
      print(message2print)
    
    
      message2print <- "COMPUTER: "
      if(computer.numb.victories>0) {
        for (i in 1:computer.numb.victories) {
          message2print <- paste(message2print,"*",sep="")
        }
      }
      print(message2print)
    } #End Loop For
  
    if(player.numb.victories < computer.numb.victories) {
      print(paste("Difficult game is over, final score: player",
                player.numb.victories, 
                "-", 
                computer.numb.victories,"computer – The COMPUTER won!"))
      play.it(i.won)
    } else if (player.numb.victories > computer.numb.victories) {
        print(paste("Difficult game is over, final score: ",  value.name,
                  player.numb.victories, 
                  "-", 
                  computer.numb.victories,"computer – You won!"))
        play.it(you.won)
    } else {
      print(paste("Easy game is over, final score: ",  value.name,
                  player.numb.victories, 
                  "-", 
                  computer.numb.victories,"It was a tie!"))
      play.it(try.again)
      print("Play again against the computer and see if you are able to beat it!")
    }
  }
  
  if (first.greeting.time == 1)  {
    play.it(another.game)
    first.greeting.time <-0
  } 
  
  keep.playing <- toupper(readline(prompt=  paste(value.name, ", Do you want to play again? (Y/N) :") ))
}

play.it(gameover)


                     
                              
