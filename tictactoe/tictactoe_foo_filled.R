# QLearning
# - setup (state x action) matrix with Q-value of 0
# - set hyperparameters
# - Q-Learning algorithms
#stateEnums = sort(unique(as.character(df$state)))
run_qlearning <- function(df){
  stateEnums = sort(unique(c(as.character(df$state), as.character(df$nextState))))
  actionEnums = sort(unique(as.character(df$action)))
  qmatrix = matrix(data = -20, nrow = length(stateEnums), ncol = length(actionEnums))
  notConverged = TRUE
  qGamma = 0.9
  qAlpha = 0.8
  eps = 0.0
  while(notConverged){
    oldQmatrix = qmatrix
    for(index in 1:nrow(df)){
      index_s = (stateEnums == df$state[index])
      index_sp = (stateEnums == df$nextState[index])
      index_a = (actionEnums == df$action[index])
      oldValue = qmatrix[index_s, index_a]
      if(runif(n=1)<eps){
        newValue = df$reward[index] + qGamma * sample(x=qmatrix[index_sp,], size=1)          
      }else{
        newValue = df$reward[index] + qGamma * qmatrix[index_sp,our_which_max_(qmatrix[index_sp,])]
      }
      qmatrix[index_s, index_a] = oldValue + qAlpha * (newValue - oldValue)
    }
    maxDiffQmatrix = max(abs(qmatrix - oldQmatrix))
    print(maxDiffQmatrix)
    notConverged = (maxDiffQmatrix > 1e-3)
  }
  return(list('stateEnums'=stateEnums, 'actionEnums'=actionEnums, 'qsa'=qmatrix))
}


our_which_max <- function(n, x, return_all = TRUE){
  ind = which(x == max(x))
  if(length(ind) == 1 | return_all){
    return(n[ind])
  }else{
    return("")
    # return(sample(x=n[ind], size=1))
  }
}

our_which_max_ <- function(x, return_all = FALSE){
  ind = which(x == max(x))
  if(length(ind) == 1 | return_all){
    return(ind)
  }else{
    return(sample(x=ind, size=1))
  }
}

# RUN CHECK
# ind = which((substr(ql$stateEnums, 1, 1) == "X") &
#               (substr(ql$stateEnums, 4, 4) == "X") &
#               (substr(ql$stateEnums, 7, 7) == "_"))
# unflattenGameboard(ql$stateEnums[ind][1])
# recommendations[ind[1]][[1]] == "1_3"

run_montecarlo <- function(){
  
  agentPolicy = randomPolicy
  
  iter = 0
  while(iter <= 10){
    
    iter = iter+1
    print('Step\t', iter)
    
    # run a few simulations
    gameSimulations = list()
    N <- 1000
    pb <- txtProgressBar(min = 0, max = N)
    for(i in 1:N){
      setTxtProgressBar(pb, value = i)
      o <- simulateTicTacToeGame(agentPolicy, randomPolicy, whoStarts = "agent") %>% 
        as_tibble() %>% 
        mutate(simu = i) %>%
        mutate(reward_of_simu = tail(reward, n=1))
      gameSimulations[[i]] <- o
    }
    gameSimulations = bind_rows(gameSimulations)
    
    # estimate V(s) by averaging returns
    running_V_estimate <- gameSimulations %>%
      group_by(state, action) %>%
      summarise(m = mean(reward_of_simu)) %>%
      group_by(state) %>%
      summarise(m = max(m), best_action = action[which.max(m)])
    
    agentPolicy = function(gb){
      eps = 0.1
      if(runif(n=1) < eps){
        randomPolicy(gb)
      }else{
        a = running_V_estimate %>%
          filter(state == flattenGameboard(gb)) %>%
          pull(best_action)
        if(length(a) == 0){
          randomPolicy(gb)
        }else{
          a = c(substr(a,1,1), substr(a,3,3))
          return(as.numeric(a))
        }
      }
    }
    
  }
  
  return(running_V_estimate)
  
}

plotGameboard <- function(ex){
  tibble(expand.grid(row=1:3, col=1:3)) %>%
    mutate(value=strsplit(ex, "")[[1]]) %>%
    ggplot() +
    aes(y=row, x=col, label=value)+
    geom_tile(fill="white")+
    geom_text(cex=10)+
    geom_hline(yintercept = c(1.5, 2.5))+
    geom_vline(xintercept = c(1.5, 2.5))+
    theme_minimal()+
    labs(x="", y="")+
    scale_y_reverse()+
    theme(axis.text = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
}

