# power 4 game simulator

create_empty_board <- function(n_cols, n_rows){
  return(matrix(data = " ", nrow = n_rows, ncol = n_cols))
}

flatten_gameboard <- function(gb){
  return(c(gb))
}

unflatten_gameboard <- function(gb, n_rows){
  return(matrix(gb, nrow=n_rows, byrow = FALSE))
}

play <- function(gb, column, player){
  idx = which(gb[,column]==" ")
  if(length(idx) == 0) stop()
  gb[idx[length(idx)], column] = player
  return(gb)
}

status <- function(gb, player){
  # check if player has won
  idx <- which(gb == player, arr.ind = TRUE)
  i = 1
  has_won = FALSE
  while(not_done){
    if(idx[i,2]<=ncol(gb)-4+1){
      if(all(gb[idx[i,1], (idx[i,2]+1:3)] == player)){
        has_won = TRUE # won horizontally
        break
      }
    }
    if(idx[i,1]<=nrow(gb)-4+1){
      if(all(gb[(idx[i,1]+1:3), idx[i,2]] == player)){
        has_won = TRUE # won vertically
        break
      }
    }
    if((idx[i,1]<=nrow(gb)-4+1) & (idx[i,2]<=ncol(gb)-4+1)){
      if((gb[idx[i,1]+1, idx[i,2]+1] == player) & (gb[idx[i,1]+2, idx[i,2]+2] == player) & (gb[idx[i,1]+3, idx[i,2]+3] == player)){
        has_won = TRUE
        break
      }
    }
    if((idx[i,1]>=4+1) & (idx[i,2]<=ncol(gb)-4+1)){
      if((gb[idx[i,1]-1, idx[i,2]+1] == player) & (gb[idx[i,1]-2, idx[i,2]+2] == player) & (gb[idx[i,1]-3, idx[i,2]+3] == player)){
        has_won = TRUE
        break
      }
    }
    i = i+1
    not_done = (i < nrow(idx)) & !has_won
  }
  return(has_won)
}

N_COLS = 5
N_ROWS = 4

gb <- create_empty_board(n_cols=N_COLS, n_rows = N_ROWS)

gb <- init_gb(play_sequence = "12234334141")
status(play(gb, 4, "R"), "R")

3^(5*4)

# autonomous play
#
#
auto_pilot <- TRUE
gb <- create_empty_board(n_cols=N_COLS, n_rows = N_ROWS)
gb <- init_gb(play_sequence = "1223")
cp <- "R"
has_won <- FALSE
not_done <- TRUE
while(not_done){
  print(paste0("BOARD, player: ", cp))
  print(gb)
  # check best moves under mini-max
  v <- sapply(X = 1:N_COLS, FUN = function(j){
    if(gb[1,j]!=" "){
      return(+Inf)
    }else{
      negamax_prune(gb = play(gb, j, cp), alpha = -Inf, beta = +Inf, verbose = FALSE)  
    }
  })
  print(v)
  if(auto_pilot | (!auto_pilot & cp == "R")){
    idx <- which.min(v)
  }else{
    idx <- as.integer(readline(prompt="Which column do you choose ?"))
  }
  gb <- play(gb, idx, cp)
  print(paste0("Chose column #", idx))
  # check winning condition
  if(status(gb = gb, player = cp)){
    has_won <- TRUE
    print(paste0("\n\n Player ", cp, " has won !!"))
    break
  }else{
    # change player
    if(cp == "R"){
      cp <- "B"
    }else{
      cp <- "R"
    }
  }
}

init_gb <- function(play_sequence){
  play_sequence <- strsplit(x=play_sequence, split="")[[1]]
  gb <- create_empty_board(n_cols=N_COLS, n_rows = N_ROWS)
  current_player <- "R"
  idx = 1
  not_done <- TRUE
  while(not_done){
    # make move
    gb <- play(gb, column = as.integer(play_sequence[idx]), player = current_player)
    #check win
    if(status(gb=gb, player=current_player)){
      break
    }
    # switch players
    if(current_player=="R"){
      current_player="B"
    }else{
      current_player="R"
    }
    # check stop condition
    idx = idx+1
    not_done <- idx<=length(play_sequence)
  }
  return(gb)
}


negamax <- function(gb){
  
  if(all(gb != " ")){ # check for draw game
    return(0)
  }
  
  cp = "R"
  if(sum(gb!=" ") %% 2 == 1){
    cp = "B"
  }
  
  for(column in 1:N_COLS){
    # check if current player can win next move
    if(gb[1,column] == " "){
      if(status(gb=play(gb, column=column, player=cp), player=cp)){
        return((N_COLS*N_ROWS+1-sum(gb!=" "))/2)
      } 
    }
  }
  
  # init best score 
  bestScore = -N_COLS*N_ROWS
  
  # compute the score of all possible next move and keep the best one
  for(column in 1:N_COLS){ 
    if(gb[1,column] == " "){
      score = -negamax(play(gb, column, cp))
      if(score > bestScore) bestScore = score # keep track of best possible score so far.
    }
  }
  
  return(bestScore)
  
}

# /**
#   * Reccursively score connect 4 position using negamax variant of alpha-beta algorithm.
# * @param: alpha < beta, a score window within which we are evaluating the position.
# *
#   * @return the exact score, an upper or lower bound score depending of the case:
#   * - if actual score of position <= alpha then actual score <= return value <= alpha
# * - if actual score of position >= beta then beta <= return value <= actual score
# * - if alpha <= actual score <= beta then return value = actual score
# */
negamax_prune <- function(gb, alpha, beta, verbose=TRUE){
  
  if(verbose){
    cat(paste0(sub(pattern = " ", replacement = ".", x = flatten_gameboard(gb)), collapse=""), '\n')
  }
  
  if(all(gb != " ")){ # check for draw game
    return(0)
  }
  
  cp = "R"
  if(sum(gb!=" ") %% 2 == 1){
    cp = "B"
  }
  
  for(column in 1:N_COLS){
    # check if current player can win next move
    if(gb[1,column] == " "){
      if(status(gb=play(gb, column=column, player=cp), player=cp)){
        return((N_COLS*N_ROWS+1-sum(gb!=" "))/2)
      } 
    }
  }
  
  # init best score 
  bestScore = (N_COLS*N_ROWS-1-sum(gb!=" "))/2
  if(beta > bestScore) {
    beta = bestScore;               # there is no need to keep beta above our max possible score.
    if(alpha >= beta) return(beta)  # prune the exploration if the [alpha;beta] window is empty.
  }
  
  # compute the score of all possible next move and keep the best one
  for(column in 1:N_COLS){
    if(gb[1,column] == " "){
      score = -negamax_prune(play(gb, column, cp), -beta, -alpha, verbose=verbose)
      # // explore opponent's score within [-beta;-alpha] windows:
      # // no need to have good precision for score better than beta (opponent's score worse than -beta)
      # // no need to check for score worse than alpha (opponent's score worse better than -alpha)
      if(score >= beta) return(score)  # // prune the exploration if we find a possible move better than what we were looking for.
      if(score > alpha) alpha = score # // reduce the [alpha;beta] window for next exploration, as we only 
      # // need to search for a position that is better than the best so far.
    }
  }
  
  return(alpha)
  
}

