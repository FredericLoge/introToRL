print(2*2) 



#### Simulation of the maze
#### 0 stands for a normal cell
#### 1 the cell where there is the goal 
#### 2 the cell where the agent start
#### 3 the cells where there is an obstacle
#### 4 the cells where you get an intermediate reward 

library(stats)

SimulateAMaze = function(nbcol, nbrow, complexity){
  Maze = matrix(data = 0, nrow = nbrow, ncol = nbcol)
  Maze = as.vector(Maze)
  AgentStartCell = 1 #as.integer(runif(1, 0,nbcol))*as.integer(runif(1, 0,nbrow))
  GoalCell = nbrow*nbcol #as.integer(runif(1, 0,nbcol))*as.integer(runif(1, 0,nbrow))
  ObstacleCells = as.integer(runif(complexity*2, 0,nbrow*nbcol))
  IntermediateRewardCells = as.integer(runif((1/complexity)*8, 0,nbrow*nbcol))
  
  Maze[IntermediateRewardCells] = 4
  Maze[ObstacleCells] = 3
  Maze[GoalCell] = 1
  if (AgentStartCell!=GoalCell){
    Maze[AgentStartCell] = 2
  }else{
    Maze[AgentStartCell + 1] = 2
  }
  return(Maze)
}
nbrow = 12
nbcol = 12
complexity = 10
Maze1 <- SimulateAMaze(nbrow, nbrow, complexity)
View(matrix(Maze1, nrow = nbrow, ncol = nbcol))

ValueFctInit <- rep(-1, nbrow*nbcol)
ValueFctInit[which(Maze1==1)] <- 0
ValueFctInit[which(Maze1==3)] <- -1000
gamma_param <- 1

GlobalInformation <- matrix(data = NA, nrow = nbrow*nbcol, ncol = 3)
GlobalInformation[,c(1,2)] <- as.matrix(expand.grid(1:nbrow, 1:nbcol))
GlobalInformation[,3] <- ValueFctInit
GlobalInformation <- cbind(GlobalInformation, Maze1)
GlobalInformation_old <- GlobalInformation
GlobalInformation_old[,3] <- 0
cc <- 0
### ValueIterationAlgorithm
while (max(abs(GlobalInformation_old[,3]-GlobalInformation[,3]))>0.01){
  cc <- cc + 1
  GlobalInformation_old <- GlobalInformation
  print(cc)
  for (s in c(1:nrow(GlobalInformation))){
    if ((GlobalInformation[s, 4]!=3) && GlobalInformation[s, 4]!=1){ # we cannot be in a cell of value 3, this is an obstacle
      # all the states where we can go from s
    s_currentstate <- GlobalInformation[s,c(1,2)]
    upper <- c(s_currentstate[1]-1, s_currentstate[2])
    right <- c(s_currentstate[1], s_currentstate[2]+1)
    left <- c(s_currentstate[1], s_currentstate[2]-1)
    lower <- c(s_currentstate[1]+1, s_currentstate[2])
    
    next_possible_states = NULL
    
    if (any(upper<=0) || upper[1] > nbrow || upper[2] > nbcol){
        upper = NULL
    }else{
      if (GlobalInformation[GlobalInformation[,1]==upper[1] & GlobalInformation[,2]==upper[2], 4]==3){
        upper = NULL
      }else{
        next_possible_states = rbind(next_possible_states, upper)
      }
    }
    if (any(right<=0) || right[1] > nbrow || right[2] > nbcol){
      right = NULL
    }else{
      if (GlobalInformation[GlobalInformation[,1]==right[1] & GlobalInformation[,2]==right[2], 4]==3){
        right = NULL
      }else{
        next_possible_states = rbind(next_possible_states, right)
      }
    }
    if (any(left<=0) || left[1] > nbrow || left[2] > nbcol){
      left = NULL
    }else{
      if (GlobalInformation[GlobalInformation[,1]==left[1] & GlobalInformation[,2]==left[2], 4]==3){
        left = NULL
      }else{
        next_possible_states = rbind(next_possible_states, left)
      }
    }
    if (any(lower<=0) || lower[1] > nbrow || lower[2] > nbcol){
      lower = NULL
    }else{
      if (GlobalInformation[GlobalInformation[,1]==lower[1] & GlobalInformation[,2]==lower[2], 4]==3){
        lower = NULL
      }else{
        next_possible_states = rbind(next_possible_states, lower)
      }
    }
    next_possible_states = matrix(next_possible_states, nrow=length(next_possible_states)/2, ncol = 2)    
    
    tmp <- NULL
    for (i in c(1:nrow(next_possible_states))){
      s_next = next_possible_states[i,]
      tmp[i] <- GlobalInformation[GlobalInformation[,1]==s_next[1] & GlobalInformation[,2]==s_next[2],3]
    }
    GlobalInformation[s, 3] <- -1 + max(tmp)
  }
  } 
}

View(matrix(data = GlobalInformation[, 3], nbrow, nbcol))

