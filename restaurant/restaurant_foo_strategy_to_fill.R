# STRATEGIES =====================================================================

pick_action <- function(historic, round_index, strategy, nb_actions){
  if(strategy$name == "RANDOM"){
    random_strategy(n=nb_actions)
  }else if(strategy$name == "EPS_GREEDY"){
    eps_greedy_strategy(n=nb_actions, historic=historic, eps=strategy$pars$eps)
  }else if(strategy$name == "ETC"){
    etc_strategy(n=nb_actions, historic=historic)
  }else if(strategy$name == "EXP3"){
    exp3_strategy(n=nb_actions, strategy) 
  }else if(strategy$name == "UCB"){
    ucb_strategy(n=nb_actions, historic=historic) 
  }else if(strategy$name == "TS"){
    ts_strategy(n=nb_actions, historic=historic) 
  }else{
    stop('strategy name unrecognized.')
  }
}

exp3_strategy <- function(n, ...){
  ...
}

random_strategy <- function(n){
  sample(x = n, size = 1)
}

eps_greedy_strategy <- function(n, historic, eps){
  if(runif(n=1)<eps){
    # explore
    sample(x = n, size = 1)
  }else{
    # exploit
    historic %>% 
      group_by(action) %>% 
      summarise(m = mean(reward)) %>% 
      slice_max(order_by = m) %>% 
      pull(action)
  }
}

etc_strategy <- function(n, historic, ...){
  ...
}

ucb_strategy <- function(n, historic, ...){
  ...
}

ts_strategy <- function(n, historic,...){
  ...
}
