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

gameSimulations = list()
N <- 10000
pb <- txtProgressBar(min = 0, max = N)
for(i in 1:N){
  setTxtProgressBar(pb, value = i)
  o <- simulateTicTacToeGame(randomPolicy, randomPolicy, whoStarts = "agent")
  o <- o %>% as_tibble() %>% mutate(simu = i)
  gameSimulations[[i]] <- o
}
gameSimulations = bind_rows(gameSimulations)

# focus on one of the players performance 
df <- gameSimulations

# QLearning
# - setup (state x action) matrix with Q-value of 0
# - set hyperparameters
# - Q-Learning algorithms
#stateEnums = sort(unique(as.character(df$state)))
stateEnums = sort(unique(c(as.character(df$state), as.character(df$nextState))))
actionEnums = sort(unique(as.character(df$action)))
qmatrix = matrix(data = -20, nrow = length(stateEnums), ncol = length(actionEnums))
notConverged = TRUE
qGamma = 0.8
qAlpha = 0.8
while(notConverged){
  oldQmatrix = qmatrix
  for(index in 1:nrow(df)){
    index_s = (stateEnums == df$state[index])
    index_sp = (stateEnums == df$nextState[index])
    index_a = (actionEnums == df$action[index])
    oldValue = qmatrix[index_s, index_a]
    newValue = df$reward[index] + qGamma * max(qmatrix[index_sp,])
    qmatrix[index_s, index_a] = oldValue + qAlpha * (newValue - oldValue)
  }
  maxDiffQmatrix = max(qmatrix - oldQmatrix)
  print(maxDiffQmatrix)
  notConverged = (maxDiffQmatrix > 1e-2)
}

table(apply(qmatrix, 1, function(x) any(x > -20)))

our_which_max <- function(n, x, return_all = TRUE){
  ind = which(x == max(x))
  if(length(ind) == 1 | return_all){
    return(n[ind])
  }else{
    return('')
  }
  
}

recommendations <- apply(qmatrix, 1, function(x) our_which_max(n=actionEnums, x = x))

ind = which(stateEnums == flattenGameboard(emptyGameboard()))
ind = which((substr(stateEnums, 1, 1) == "X") & 
        (substr(stateEnums, 4, 4) == "X") &
  (substr(stateEnums, 7, 7) == "_"))
unflattenGameboard(stateEnums[ind][1])
rec = tibble(a = actionEnums, q = qmatrix[ind[1],]) %>%
  mutate(row = substr(a, 1, 1), 
         col = substr(a, 3, 3))
recommendations[ind[1]]
ggplot(data = rec) +
  aes(x = col, y = row, fill = q) +
  geom_tile() + 
  scale_fill_viridis_c()

