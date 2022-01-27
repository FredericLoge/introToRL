# QLearning
run_qlearning <- function(df){
  # define state space
  # define action space
  # initialize Qmatrix
  while(...){
    # iterate through each state
    for(...){
      # update Qvalue estimate
    }
    # check for convergence
  }
  return(...)
}

# Monte Carlo
run_montecarlo <- function(){
  
  # instanciate baseline agent policy
  agentPolicy = randomPolicy
  iter = 0
  while(iter <= n_iter){
    
    # run some simulations under current policy
    ...
    # estimate V(s) by averaging returns
    ...
    # redefine policy
    ...
  }
  
  return(...)
  
}

