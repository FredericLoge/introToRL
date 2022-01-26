# STRATEGIES =====================================================================

pick_action <- function(historic, round_index, strategy, nb_actions){
  if(strategy$name == "RANDOM"){
    random_strategy(n=nb_actions)
  }else if(strategy$name == "EPS_GREEDY"){
    eps_greedy_strategy(n=nb_actions, historic=historic, strategy=strategy)
  }else if(strategy$name == "ETC"){
    etc_strategy(n=nb_actions, historic=historic, round_index=round_index, strategy=strategy)
  }else if(strategy$name == "EXP3"){
    exp3_strategy(n=nb_actions, strategy) 
  }else if(strategy$name == "UCB"){
    ucb_strategy(n=nb_actions, historic=historic, alpha=strategy$pars$alpha) 
  }else if(strategy$name == "TS"){
    ts_strategy(n=nb_actions, historic=historic) 
  }else{
    stop('strategy name unrecognized.')
  }
}

exp3_strategy <- function(n, strategy){
  w=strategy$pars$weights
  p = w/sum(w)
  sample(x=n, size=1, prob=p)
}

random_strategy <- function(n){
  sample(x = n, size = 1)
}

eps_greedy_strategy <- function(n, historic, strategy){
  eps=strategy$pars$eps
  if(runif(n=1)<eps){
    random_strategy(n=n)
  }else{
    o <- historic %>% 
      group_by(action) %>% 
      summarise(mr = mean(reward))
    sample_from_max(action = o$action, value = o$mr)
  }
}

etc_strategy <- function(n, historic, round_index, strategy){
    o <- historic %>%
      filter(time_index %in% 1:(n*strategy$pars$nb_cold_start_rounds)) %>% 
      group_by(action) %>% 
      summarise(mr = mean(reward))
    sample_from_max(action = o$action, value = o$mr)
}

ucb_strategy <- function(n, historic, alpha){
  o <- historic %>% 
    group_by(action) %>%
    summarise(nb = as.numeric(n()), n0 = sum(reward),
              hb_m = get_CI(n = nb, s = n0, alpha = 0.05)$auto[2]
    )
  sample_from_max(action = o$action, value = o$hb_m)
}

ts_strategy <- function(n, historic){
  o <- historic %>%
    group_by(action) %>%
    summarise(n = n(), n0 = sum(reward)) %>%
    mutate(mu_r = rbeta(n = n, shape1 = 1+n0, shape2 = 1 + (n-n0)))
  sample_from_max(action = o$action, value = o$mu_r)
}

sample_from_max <- function(action, value){
  ind = which(value == max(value))
  if(length(ind) > 1){
    action[sample(x = ind, size = 1)]
  }else{
    action[ind]
  }
}
