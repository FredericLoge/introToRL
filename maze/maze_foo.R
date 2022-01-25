simulate_maze = function(nbcol, nbrow, complexity, goal_reward, obstacle_reward, immediate_reward, usual_reward){

  prob <- c(2*complexity, 40/complexity, NA)
  prob[3] <- nbrow*nbcol - (prob[1]+prob[2])
  
  maze <- expand.grid(row = 1:nbrow, col = 1:nbcol) %>%
    as_tibble() %>%
    mutate(index = 1:n(), .before = 'row') %>%
    mutate(category = sample(x = c("obstacle", "intermediate_reward", "normal"), size = n(), replace = TRUE, prob = prob)) %>%
    mutate(category = case_when(
      row==1 & col==1 ~ "start",
      row==nbrow & col==nbcol ~ "goal",
      TRUE ~ category
    )) %>%
    mutate(
      reward = case_when(
        category == 'goal' ~ +100,
        category == 'obstacle' ~ -1000,
        category == 'intermediate_reward' ~ 0,
        TRUE ~ -5
      )
    )
  return(maze)
}


plot_maze_environment <- function(states){
  nn <- max(states$row)
  ll <- c('start', 'intermediate_reward', 'normal', 'obstacle', 'goal')
  states %>%
    mutate(category = factor(category, levels=ll)) %>%
    ggplot() +
    aes(x=factor(col), y = factor(row, levels = nn:1)) +
    geom_tile(aes(fill=category), col='white', lwd=2) +
    geom_text(data=states %>% filter(category %in% c('obstacle', 'intermediate_reward')),
              aes(label=emoji(aliases=ifelse(category =='obstacle', 'building_construction', "chocolate_bar"))), 
              family='EmojiOne', cex=15, vjust=0.25) +
    scale_x_discrete(position="top") +
    coord_equal() +
    labs(x='', y='', fill='', title='Maze environment') +
    theme_minimal() +
    theme(legend.position='bottom', 
          panel.grid=element_blank(),
          axis.text=element_text(size=20),
          plot.title=element_text(size=25),
          legend.text=element_text(size=15))
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

our_which_max <- function(named_vec, return_all=FALSE){
  ind = which(named_vec == max(named_vec))
  if(length(ind) == 1){
    return(names(named_vec)[ind])
  }else{
    if(return_all){
      return(names(named_vec)[ind])
    }else{
      return('')
    }
  }
}


run_value_function_iteration <- function(maze, pars = list(delta_threshold = 0.01, max_iter = 100, gamma = 0.9)){
  
  value_function <- maze %>%
    mutate(
      current_estimate = reward,
      old_estimate = NA,
      recommended_action = ''
    )
  
  delta_threshold = pars$delta_threshold
  max_iter = pars$max_iter
  gamma = pars$gamma
  
  cc <- 0
  still_going <- TRUE
  n_chars = 10
  
  # log
  vv = c('Step', 'Ratio', 'Delta')
  cat(log_rule(length(vv), n_chars))
  cat(log_print(vv, n_chars))
  cat(log_rule(length(vv), n_chars))
  
  while (still_going){
    # update counter, and old estimate
    cc <- cc + 1
    value_function$old_estimate = value_function$current_estimate
    for (s in c(1:nrow(value_function))){
      if(value_function$category[s] %in% c('normal', 'start', 'intermediate_reward')){
        # all the states where we can go from s
        s_currentstate <- value_function[s, c('row', 'col')] %>% unlist()
        next_possible_states <- list(
          'upper' = c(s_currentstate[1]-1, s_currentstate[2]),
          'right' = c(s_currentstate[1], s_currentstate[2]+1),
          'left' = c(s_currentstate[1], s_currentstate[2]-1),
          'lower' = c(s_currentstate[1]+1, s_currentstate[2])
        )
        # identify old estimate from next possible states
        o <- purrr::map(next_possible_states, function(x){
          value_function %>%
            filter(row == x[1], col == x[2]) %>%
            pull(old_estimate)
        })
        # update current estimate
        value_function$current_estimate[s] <- value_function$reward[s] + gamma * max(unlist(o))
        # check out recommended action
        value_function$recommended_action[s] <- our_which_max(unlist(o), return_all = FALSE)
      }
    } 
    
    # decide whether delta is sufficiently low to stop 
    delta = max(abs(value_function$old_estimate - value_function$current_estimate))
    still_going = (delta > delta_threshold) 
    
    # if gone over max iteration nb, stop
    if(cc > max_iter){ still_going = FALSE }
    
    # log
    nb = sum(value_function$recommended_action != "")
    nb_ = sum(value_function$category %in% c('normal', 'start', 'intermediate_reward'))
    vv = c(as.integer(cc), paste0(as.integer(nb), "/", as.integer(nb_)), round(delta,2))
    cat(log_print(vv, n_chars))
    
  }
  
  return(value_function)
  
}


plot_maze_policy <- function(value_function){
  
  my_list <- list()
  for (s in c(1:nrow(value_function))){
    if(value_function$category[s] %in% c('normal', 'start', 'intermediate_reward')){
      s_currentstate <- value_function[s, c('row', 'col')] %>% unlist()
      next_possible_states <- list(
        'upper' = c(s_currentstate[1]-1, s_currentstate[2]),
        'right' = c(s_currentstate[1], s_currentstate[2]+1),
        'left' = c(s_currentstate[1], s_currentstate[2]-1),
        'lower' = c(s_currentstate[1]+1, s_currentstate[2])
      )
      o <- purrr::map(next_possible_states, function(x){
        value_function %>%
          filter(row == x[1], col == x[2]) %>%
          pull(old_estimate)
      })
      o <- our_which_max(unlist(o), return_all = TRUE)
      
      my_list[[length(my_list)+1]] <- tibble('row' = value_function$row[s], 
                                             'col' = value_function$col[s], 
                                             'recommended_action' = o)
    }
  }    
  w = 0.5
  my_list = bind_rows(my_list) %>%
    mutate(col = as.numeric(col), row = as.numeric(row)) %>%
    mutate(
      x = case_when(
        recommended_action == "right" ~ col,
        recommended_action == "left" ~ col+w,
        TRUE ~ as.numeric(col)
      ),
      xend = case_when(
        recommended_action == "right" ~ col+w,
        recommended_action == "left" ~ col,
        TRUE ~ as.numeric(col)
      ),
      y = case_when(
        recommended_action == "lower" ~ row,
        recommended_action == "upper" ~ row+w,
        TRUE ~ as.numeric(row)
      ),
      yend = case_when(
        recommended_action == "lower" ~ row+w,
        recommended_action == "upper" ~ row,
        TRUE ~ as.numeric(row)
      )
    )
  
  nn <- max(value_function$row)
  ll <- c('start', 'intermediate_reward', 'normal', 'obstacle', 'goal')
  value_function %>%
    mutate(category = factor(category, levels=ll)) %>%
    ggplot() +
    aes(x=col, y = row) +
    geom_tile(aes(fill=category), col='white', lwd=2) +
    geom_text(data=value_function %>% filter(category %in% c('obstacle', 'intermediate_reward')),
              aes(label=emoji(aliases=ifelse(category =='obstacle', 'building_construction', "chocolate_bar"))), 
              family='EmojiOne', cex=15, vjust=0.25) +
    geom_segment(data = my_list %>% filter(!recommended_action %in% c('')), 
                 aes(x = x, xend = xend, y = y, yend = yend),
                 arrow = arrow(length = unit(0.5, "cm"))) +
    scale_x_continuous(position="top", breaks=1:12) +
    scale_y_reverse(breaks=1:12) +
    coord_equal() +
    labs(x='', y='', fill='', title='Maze environment') +
    theme_minimal() +
    theme(legend.position='bottom', 
          panel.grid=element_blank(),
          axis.text=element_text(size=20),
          plot.title=element_text(size=25),
          legend.text=element_text(size=15))
}
