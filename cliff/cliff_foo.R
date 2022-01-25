#' @title Generate states tibble
#' @param n number of rows
#' @param m number of cols
#' @return states tibble
generate_states <- function(n, m, goal_reward, abyss_reward, default_reward=-1){
  
  states = expand.grid('col'=1:m, 'row'=1:n) %>%
    tibble() %>%
    mutate(index = 1:n()) %>%
    mutate(category = case_when(
      index == (n-1)*m+1 ~ 'start',
      index == n*m ~ 'goal',
      index %in% ((n-1)*m+2):(n*m-1) ~ 'abyss',
      TRUE ~ 'grass'
    )) %>% 
    mutate(is_terminal = category %in% c('abyss', 'goal')) %>%
    mutate(reward = case_when(
      category == "abyss" ~ abyss_reward,
      category == "goal" ~ goal_reward,
      TRUE ~ default_reward
    ))
  
  return(states)
  
}


#' @title Generate Transition Matrix
#' @param s states
#' @param p wind parameter
#' @return transition matrix
generate_transition_matrix <- function(s, pr){
  
  n = max(s$row)
  m = max(s$col)
  
  tmat = expand.grid('state0' = s$index, 
                     'action' = c('north', 'south', 'east', 'west'),
                     'state1' = s$index) %>%
    tibble() %>%
    left_join(s %>% select(c(index, reward)), 
              by = c('state1'='index')) %>%
    mutate(p = pr^{n + 1 - ceiling(state0/m)}) %>%
    mutate(prob = case_when(
      #   if a == 'South':
      #     P(S_{t+1} = s + m | S_t = s, A_t = a) = 1
      (action == 'south') & ((state0 + m) == state1) ~ 1,
      (action == 'south') & (state0 == state1) & (state0 >= (n-1)*m+1) ~ 1,
      
      #   else if a == 'North':
      #     P(S_{t+1} = s | S_t = s, A_t = a) = p   # sur place
      (action == 'north') & (state0 == state1) & (state0 >m) ~ p,
      #     P(S_{t+1} = s-m | S_t = s, A_t = a) = 1-p # va au Nord
      (action == 'north') & ((state0-m) == state1) ~ 1-p,
      (action == 'north') & (state0 == state1) & (state0 <= m) ~ 1,
      
      #   else:
      #     P(S_{t+1} = s + m | S_t = s, A_t = a) = p  # va au Sud
      #     P(S_{t+1} = s + 1*1{a = 'East'} - 1*1{a = 'West'} | S_t = s, A_t = a) = 1-p # va a l'Ouest ou l'Est
      (action == 'east') & ((state0+m) == state1) ~ p,
      (action == 'east') & (state0 == state1) & (state0 %in% ((n-1)*m+1):(n*m-1)) ~ p,
      (action == 'east') & ((state0+1) == state1) & (state0 %% m != 0) ~ 1-p,
      (action == 'east') & (state0 == state1) & (state0 %% m == 0) & (state0 < n*m) ~ 1-p,
      (action == 'east') & (state0 == state1) & (state0 == n*m) ~ 1,

      # WEST
      (action == 'west') & (state0 == state1) & (state0 == (n-1)*m+1) ~ 1,
      (action == 'west') & ((state0+m) == state1) ~ p,
      (action == 'west') & (state0 == state1) & (state0 %in% ((n-1)*m+2):(n*m)) ~ p,
      (action == 'west') & ((state0-1) == state1) & (state0 %% m != 1) ~ 1-p,
      (action == 'west') & (state0 == state1) & (state0 %% m == 1) ~ 1-p,
      
      TRUE ~ 0
      
    )
  )
  
  return(tmat)
  
}


#' @title Check that generated transition matrix is correct
#' @param tmat transition matrix, as defined in above function
#' @return boolean indicating if test is OK
check_integrity_transition_matrix <- function(tmat){
  cat("Checking that sum_{s'} P(S_{t+1} = s' | S_t = s, A_t = a) = 1.\n")
  tmat_ = tmat %>%
    group_by(state0, action) %>% 
    summarise(sum_prob = sum(prob), .groups = 'drop') %>%
    filter(sum_prob != 1) 
  if(nrow(tmat_) == 0){
    cat("All good.\n")
    return(TRUE)
  }else{
    cat("Mmmh. Following couples (state0, action) have inadequate probability sum:\n")
    print(tmat_)
    return(FALSE)
  }
}


#' @title Plot cliff environment
#' @param states states of cliff problem
#' @return ggplot of environment
plot_cliff_environment <- function(states){
  ggplot(data=states) +
    geom_tile(aes(x=col, y=row, fill=category), col='white', lwd=2) +
    geom_text(data=states %>% filter(category=='abyss'),
              aes(x=col, y=row),
              label=emoji(aliases='skull_and_crossbones'), 
              family='EmojiOne', cex=25, vjust=0.25) +
    scale_y_reverse() +
    scale_x_continuous(position="top", breaks=1:max(states$col)) +
    coord_equal() +
    labs(x='', y='', fill='', title='Cliff environment') +
    theme_minimal() +
    theme(legend.position='bottom', 
          panel.grid=element_blank(),
          axis.text=element_text(size=20),
          plot.title=element_text(size=25),
          legend.text=element_text(size=15))
}


best_action <- function(action, value){
  o = which(value == max(value))
  if(length(o)>1){
    'multiple'
  }else{
    action[o]
  }
}

log_print <- function(vv, nb=10){
  a = sapply(vv, function(v){
    l = nchar(v)
    paste0(paste0(rep(" ", times = max(nb-l,0)), collapse = ''), v)
  })
  paste0('\n\t|', paste0(a, collapse='|'), '|')
}

log_rule <- function(n, nb=10){
  a = sapply(1:n, function(i) paste0(rep("-", times = nb), collapse = ''))
  paste0('\n\t|', paste0(a, collapse='|'), '|')
}

run_value_iteration <- function(states, tmat, pars_VI, pars_print){
  
  gamma=pars_VI$gamma
  delta_threshold=pars_VI$delta_threshold 
  max_iter=pars_VI$max_iter
  n_chars=pars_print$n_chars
  
  vv = c('Step', 'Ratio', 'Delta')
  cat(log_rule(length(vv), n_chars))
  cat(log_print(vv, n_chars))
  cat(log_rule(length(vv), n_chars))

  # add action recommendation
  states$recommended_action = factor(x = 'undetermined', levels = c('undetermined', 'not_applicable', 'north', 'south', 'west', 'east'))
  
  # add Bellman value estimate variable, instanciate with 0
  states$value = 0
  
  # history of value iteration estimates 
  va_history = NULL
  
  # iterate until Bellman value estimate has not change much
  step = 0
  va_delta = 0
  va_changed = TRUE
  while(va_changed){

    step = step + 1    
    
    # add to history
    va_history = cbind(va_history, states$value)
    
    # new value function estimate and best action 
    tmp = tmat %>% 
      left_join(states %>% select(index, value), by = c('state1'= 'index')) %>% 
      left_join(states %>% select(index, is_terminal), by = c('state0'= 'index')) %>% 
      group_by(state0, is_terminal, action) %>%
      summarise('value' = sum(prob * (reward + gamma * value)), .groups='drop') %>%
      group_by(state0, is_terminal) %>%
      summarise('recommended_action' = best_action(action, value),
                'value' = max(value), .groups='drop') %>%
      mutate(value = case_when(is_terminal ~ 0, TRUE ~ value),
             recommended_action = case_when(!is_terminal ~ recommended_action, TRUE ~ 'not_applicable'))

    # save results in states
    states$recommended_action = tmp$recommended_action
    states$value = tmp$value
    
    # evaluate delta in value estimate
    va_delta = max(abs(tmp$value - va_history[,ncol(va_history)]))
    
    # decide whether delta is sufficiently low to stop 
    va_changed = (va_delta > delta_threshold) 
    
    # print log
    nb = sum(states$recommended_action %in% c('west', 'east', 'north', 'south'))
    vv = c(as.integer(step),
           paste0(as.integer(nb), "/", as.integer(sum(!states$is_terminal))), 
           round(va_delta,2))
    cat(log_print(vv, n_chars))

    # 
    if(!va_changed){
      va_changed = (nb < sum(!states$is_terminal))
    }
    
    if(step > max_iter){
      va_changed = FALSE
    }
    
  }
  cat(log_rule(length(vv), n_chars))
  
  return(list('states' = states, 'pars' = pars_VI))
  
}


plot_cliff_policy <- function(vi_out){
  
  # identify (x, xend, y, yend) of arrows to draw afterwards
  vi_output = vi_out$states %>%
    mutate(
      x = case_when(
        recommended_action == "east" ~ col-0.25,
        recommended_action == "west" ~ col+0.25,
        TRUE ~ as.numeric(col)
      ),
      xend = case_when(
        recommended_action == "east" ~ col+0.25,
        recommended_action == "west" ~ col-0.25,
        TRUE ~ as.numeric(col)
      ),
      y = case_when(
        recommended_action == "south" ~ row-0.25,
        recommended_action == "north" ~ row+0.25,
        TRUE ~ as.numeric(row)
      ),
      yend = case_when(
        recommended_action == "south" ~ row+0.25,
        recommended_action == "north" ~ row-0.25,
        TRUE ~ as.numeric(row)
      )
    )
  
  # ggplot
  ggplot(data=vi_output) +
    geom_tile(aes(x = col, y = row, fill = category), col='white', lwd=2) +
    geom_text(data = vi_output %>% filter(category == 'abyss'),
              aes(x = col, y = row-0.25),
              label=emoji(aliases = 'skull_and_crossbones'), 
              family='EmojiOne', cex = 25) +
    geom_segment(data = vi_output %>% filter(recommended_action %in% c('east', 'west', 'south', 'north')), 
                 aes(x = x, xend = xend, y = y, yend = yend),
                 arrow = arrow(length = unit(0.5, "cm"))) +
    scale_y_reverse() +
    scale_x_continuous(position="top", breaks=1:max(states$col)) +
    coord_equal() +
    labs(x = '', y = '', fill = '', title = 'Optimal actions', 
         subtitle = expr(paste("Under parameters: ", gamma, "=", !!vi_out$pars$gamma, ", ", Delta, "=", !!vi_out$pars$delta_threshold, ", max_iter = ", !!vi_out$pars$max_iter))) +
    theme_minimal() +
    theme(legend.position = 'bottom', 
          panel.grid = element_blank(),
          axis.text = element_text(size=20),
          plot.title = element_text(size = 25),
          plot.subtitle = element_text(size = 20),
          legend.text = element_text(size = 15))
}


simulate_run <- function(vi_output){
  position = tibble('state0'=vi_output$states$index[vi_output$states$category=="start"], 'action'='?', 'state1'=NA)
  not_done = TRUE
  while(not_done){
    # check best possible action
    position$action[nrow(position)] = vi_output$states %>% 
      filter(index == position$state0[nrow(position)]) %>%
      pull(recommended_action)
    # sample path
    potentials <- tmat %>%
      filter(state0 == position$state0[nrow(position)],
             action == position$action[nrow(position)],
             prob > 0)
    # record new state
    he <- potentials$state1[sample(x = nrow(potentials), size = 1, prob = potentials$prob)]
    position$state1[nrow(position)] <- he
    if(he %in% states$index[states$is_terminal]){
      not_done = FALSE     
      position = bind_rows(position, tibble('state0'=he, 'action'='none, arrived', 'state1'=NA))
    }else{
      position = bind_rows(position, tibble('state0'=he, 'action'='?', 'state1'=NA))
    }
  }
  position = position %>%
    left_join(vi_output$states %>% select(index, col, row), by = c('state0'='index')) %>% 
    mutate('time_index'=1:n())
  return(position)
}


