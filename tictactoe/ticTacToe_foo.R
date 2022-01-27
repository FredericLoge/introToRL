emptyGameboard <- function(){
  matrix(data = "_", nrow = 3, ncol = 3)  
}

randomGameboard <- function(){
  matrix(data = sample(c("_", "X", "O"), size = 3*3, replace = TRUE),
         nrow = 3, ncol = 3)
}

checkIfPlayerWon <- function(gb, mark){
  condRows = any(rowSums(gb == mark) == 3)
  condCols = any(colSums(gb == mark) == 3)
  condDiag1 = sum(sapply(1:3, function(i) gb[i,i]) == mark) == 3
  condDiag2 = sum(sapply(1:3, function(i) gb[3-i+1,i]) == mark) == 3
  return(condRows | condCols | condDiag1 | condDiag2)
}

randomPolicy <- function(gb){
  freeSpace = which(gb == "_", arr.ind = TRUE)
  return(freeSpace[sample(1:nrow(freeSpace), 1),])
}

flattenGameboard <- function(gb){
  return(paste0(c(gb), collapse=''))
}

unflattenGameboard <- function(flattengb){
  flattengb_str = strsplit(flattengb, "")[[1]]
  return(matrix(flattengb_str, nrow = sqrt(length(flattengb_str)), byrow = FALSE))
}
# NOT RUN:
# some_gb = randomGameboard()
# unflattenGameboard(flattenGameboard(some_gb))

simulateTicTacToeGame <- function(agentPolicy, robotPolicy, whoStarts="agent"){
  gb = emptyGameboard()
  gameHistory = list('state'= c(), 
                     'turn' = c(), 
                     'action' = c(), 
                     'reward' = c(), 
                     'nextState' = c())
  winner = "undecided"
  turn = whoStarts
  boardIsNotFull = TRUE
  i = 0
  while(boardIsNotFull & winner=="undecided"){
    
    if(turn == 'agent'){
      i = i + 1
      
      # only collect information here
      gameHistory$state[i] = flattenGameboard(gb)
      gameHistory$turn[i] = turn
      a = agentPolicy(gb)
      gameHistory$action[i] = paste0(a[1], "_", a[2])
      gameHistory$reward[i] = 0
      gb[a[1], a[2]] = "X"
      if(checkIfPlayerWon(gb, "X")){
        gameHistory$reward[i] = +1
        winner = 'agent'
      }else{
        gameHistory$reward[i] = 0
      }
    }else{
      a = robotPolicy(gb)
      gb[a[1], a[2]] = "O"
      if(i > 0){ gameHistory$nextState[i] = flattenGameboard(gb) }
      if(checkIfPlayerWon(gb, "O")){
        winner = 'robot'
        gameHistory$reward[i] = -1
      }
    }
    
    # change turn
    turn = ifelse(turn == "agent", "robot", "agent")    
    
    # check that board isn't full
    boardIsNotFull = (sum(gb == "_")>0)
    
  }
  
  ls = length(gameHistory$state) 
  lns = length(gameHistory$nextState) 
  if(lns == (ls - 1)){
    gameHistory$nextState[ls] = gameHistory$state[ls]
  }else if(lns < (ls-1)){
    stop('Weird dimensions on simulation log. Check run.')
  }
  
  return(gameHistory)
}
