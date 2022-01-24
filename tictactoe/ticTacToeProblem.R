# PROBLEM INSTRUCTIONS
#
# Board p x p, p = 3
# Two players, player 1 and 2
# Each player can set its mark on a cell
# Marks: "X" for player 1, "O" for player 2
# Either player can start
# When one player aligns p marks, (s)he wins and game ends.

# We'll try two different approaches to learn to play tic-tac-toe
# (1) off-policy approach
# (2) on-policy approach

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

randomPolicy(gb)

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

# We will store game history in the following way : 
# (gameboard vectorized, action, reward)
# only for player 1
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
      }else{
        gameHistory$reward[i] = 0
      }
    }else{
      a = robotPolicy(gb)
      gb[a[1], a[2]] = "O"
      if(i > 0){ gameHistory$nextState[i] = flattenGameboard(gb) }
      if(checkIfPlayerWon(gb, "0")){
        gameHistory$reward[i] = -1
      }
    }
    
    # change turn
    turn = ifelse(turn == "agent", "robot", "agent")    
    
    # check that board isn't full
    boardIsNotFull = (sum(gb == "_")>0)
  }
  
  return(gameHistory)
}

gameSimulations = list()
for(i in 1:100){
  o <- simulateTicTacToeGame(randomPolicy, randomPolicy)
  o <- data.frame(o)
  o$simulation <- i
  gameSimulations[[i]] <- o
}
gameSimulations = do.call(rbind.data.frame, gameSimulations)

# focus on one of the players performance 
df <- gameSimulations[gameSimulations$turn == "player1",]

# QLearning
# - setup (state x action) matrix with Q-value of 0
# - set hyperparameters
# - Q-Learning algorithms
stateEnums = sort(unique(as.character(df$state)))
# stateEnums = sort(unique(c(as.character(df$state), as.character(df$nextState))))
actionEnums = sort(unique(as.character(df$action)))
qmatrix = matrix(data = -20, nrow = length(stateEnums), ncol = length(actionEnums))
notConverged = TRUE
qGamma = 0.8
qAlpha = 1.0
while(notConverged){
  oldQmatrix = qmatrix
  for(index in 1:nrow(df)){
    index_s = (stateEnums == df$state[index])
    index_sp = (stateEnums == df$nextState[index])
    index_a = (actionEnums == df$action[index])
    oldValue = qmatrix[index_s, index_a]
    newValue = df$reward1[index] + qGamma * max(qmatrix[index_sp,])
    qmatrix[index_s, index_a] = oldValue + qAlpha * (newValue - oldValue)
  }
  maxDiffQmatrix = max(qmatrix - oldQmatrix)
  print(maxDiffQmatrix)
  notConverged = (maxDiffQmatrix > 1e-2)
}
table(apply(qmatrix, 1, function(x) any(x > -20)))

# 
runQLearning <- function(state, action, reward, nextState){
  
}