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
  AgentStartCell = as.integer(runif(1, 0,nbcol))*as.integer(runif(1, 0,nbrow))
  GoalCell = as.integer(runif(1, 0,nbcol))*as.integer(runif(1, 0,nbrow))
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
nbrow = 12
complexity = 20
Maze1 <- SimulateAMaze(nbrow, nbrow, complexity)
View(matrix(Maze1, nrow = nbrow, ncol = nbcol))

ValueFctInit <- rep(1, nbrow*nbcol)
ValueFctInit[which(Maze1==1)] <- 0
gamma_param <- 1

GlobalInformation <- matrix(data = NA, nrow = nbrow*nbcol, ncol = 3)
GlobalInformation[,c(1,2)] <- as.matrix(expand.grid(1:nbrow, 1:nbcol))
GlobalInformation[,3] <- ValueFctInit
### ValueIterationAlgorithm
for (s in c(1:nrow(GlobalInformation))){
  if (s!=3){ # we cannot be in a cell of value 3, this is an obstacle
    # all the states where we can go from s
    s_currentstate <- GlobalInformation[i,c(1,2)]
    upper <- c(s_currentstate[1]-1, s_currentstate[2]-1)
    next_possible_states = NULL
    if (any(upper<=0)){
      upper = NULL
    }else{
      next_possible_states = rbind(next_possible_states, upper)
    }
    right <- c(s_currentstate[1]-1, s_currentstate[2]+1)
    if (any(right<=0)){
      right = NULL
    }else{
      next_possible_states = rbind(next_possible_states, right)
    }
    left <- c(s_currentstate[1]+1, s_currentstate[2]-1)
    if (any(left<=0)){
      left = NULL
    }else{
      next_possible_states = rbind(next_possible_states, left)
    }
    lower <- c(s_currentstate[1]+1, s_currentstate[2]+1)
    if (any(lower<=0)){
      lower = NULL
    }else{
      next_possible_states = rbind(next_possible_states, lower)
    }
    next_possible_states = next_possible_states[Maze1[next_possible_states]!=3]
    next_possible_states = matrix(next_possible_states, nrow=length(next_possible_states)/2, ncol = 2)    
    for (i in c(1:nrow(next_possible_states))){
      s_next = next_possible_states[i,]
      GlobalInformation[rowSums(GlobalInformation[,c(1,2)]==s_next)>1,3]
        -1 + gamma_param*Old_ValueFct[s_next]
      }
    New_ValueFct[s] <- 
      
  }
} 




