# m x n grid
# grid indexes are as follows : 
# |         1 |   2 | ... |   m |
# |       m+1 | m+2 | ... | 2*m |
# | ...                     |
# | (n-1)*m+1 | ... | ... | n*m |
#
# last row is specially defined : 
# - start point is on index m*n+1
# - end point is on index n*m
# - points n*m+2 up to n*m-1 included are traps (cliff fall ensured)
# - wind blows to the south, such that 
#   if a == 'South':
#     P(S_{t+1} = s + m | S_t = s, A_t = a) = 1
#   else if a == 'North':
#     P(S_{t+1} = s | S_t = s, A_t = a) = p   # sur place
#     P(S_{t+1} = s-m | S_t = s, A_t = a) = 1-p # va au Nord
#   else:
#     P(S_{t+1} = s + m | S_t = s, A_t = a) = p  # va au Sud
#     P(S_{t+1} = s + 1*1{a = 'East'} - 1*1{a = 'West'} | S_t = s, A_t = a) = 1-p # va a l'Ouest ou l'Est
#
# CAREFUL WITH BOUNDARIES ====================================

n = 5
m = 6

library(tidyverse)

generate_cliff <- function(n, m, p){
  
  foo = function(x, n, m){
    v = rep('grass', length(x))
    v[x == (n-1)*m+1] = 'start'
    v[x == n*m] = 'goal'
    v[x %in% ((n-1)*m+2):(n*m-1)] = 'abyss'
    return(v)
  }
  
  states = expand.grid('col'=1:m, 'row'=1:n) %>%
    tibble() %>%
    mutate(index = 1:n()) %>%
    mutate(category = foo(index, n, m)) %>%
    mutate(is_terminal = category %in% c('abyss', 'goal')) %>%
    mutate(reward = -1)
  states$reward[states$category == "abyss"] = -20
  states$reward[states$category == "goal"] = +20
  
  action_enum = c('north', 'south', 'east', 'west')
  
  tmat = expand.grid('state0' = states$index, 
                     'action' = action_enum,
                     'state1' = states$index) %>%
    tibble() %>%
    mutate(prob = 0) %>%
    # left_join(states %>% select(-c(row, col)), by = c('state0'='index')) %>%
    left_join(states %>% select(c(index, reward)), by = c('state1'='index'))

  #   if a == 'South':
  #     P(S_{t+1} = s + m | S_t = s, A_t = a) = 1
  cond = (tmat$action == 'south') & ((tmat$state0 + m) == tmat$state1)
  tmat$prob[cond] = 1
  cond = (tmat$action == 'south') & (tmat$state0 == tmat$state1) & (tmat$state0 >= (n-1)*m+1)
  tmat$prob[cond] = 1
  
  #   else if a == 'North':
  #     P(S_{t+1} = s | S_t = s, A_t = a) = p   # sur place
  cond = (tmat$action == 'north') & (tmat$state0 == tmat$state1)
  tmat$prob[cond] = p
  #     P(S_{t+1} = s-m | S_t = s, A_t = a) = 1-p # va au Nord
  cond = (tmat$action == 'north') & ((tmat$state0-m) == tmat$state1)
  tmat$prob[cond] = 1-p
  cond = (tmat$action == 'north') & (tmat$state0 == tmat$state1) & (tmat$state0 <= m)
  tmat$prob[cond] = 1
  
  #   else:
  #     P(S_{t+1} = s + m | S_t = s, A_t = a) = p  # va au Sud
  cond = (tmat$action %in% c('east', 'west')) & ((tmat$state0+m) == tmat$state1)
  tmat$prob[cond] = p
  cond = (tmat$action %in% c('east', 'west')) & (tmat$state0 == tmat$state1) & (tmat$state0 >= (n-1)*m+1)
  tmat$prob[cond] = tmat$prob[cond] + p
  #     P(S_{t+1} = s + 1*1{a = 'East'} - 1*1{a = 'West'} | S_t = s, A_t = a) = 1-p # va a l'Ouest ou l'Est
  cond = (tmat$action == 'east') & ((tmat$state0+1) == tmat$state1) & (tmat$state0 %% m != 0)
  tmat$prob[cond] = 1-p
  cond = (tmat$action == 'east') & (tmat$state0 == tmat$state1) & (tmat$state0 %% m == 0)
  tmat$prob[cond] = 1-p
  cond = (tmat$action == 'west') & ((tmat$state0-1) == tmat$state1) & (tmat$state0 %% m != 1)
  tmat$prob[cond] = 1-p
  cond = (tmat$action == 'west') & (tmat$state0 == tmat$state1) & (tmat$state0 %% m == 1)
  tmat$prob[cond] = 1-p
  cond = (tmat$action == 'west') & (tmat$state0 == tmat$state1) & (tmat$state0 == (n-1)*m+1)
  tmat$prob[cond] = 1
  
  # cond = (tmat$is_terminal)
  # tmat$prob[cond] = 0
  # cond = (tmat$is_terminal) & (tmat$state0 == tmat$state1)
  # tmat$prob[cond] = 1
  
  list('n'=n, 'm'=m, 'p'=p, 'states'=states, 'tmat'=tmat %>% filter(prob>0))
  
}
g = generate_cliff(n=5, m=5, p=0.25)

ggplot(data=g$states) +
  aes(x = col, y = row, fill = category) +
  geom_tile() +
  scale_y_reverse()

g$tmat %>% filter(state0 == 1) %>% arrange(action)
g$tmat %>% filter(state0 == 5) %>% arrange(action)
g$tmat %>% filter(state0 == 21) %>% arrange(action)
g$tmat %>% filter(state0 == 23) %>% arrange(action)

g$tmat %>%
  group_by(state0, action) %>% 
  summarise(s = sum(prob)) %>%
  filter(s != 1)

best_action <- function(action, value){
  o = which(value == max(value))
  if(length(o)>1){
    'multiple'
  }else{
    action[o]
  }
}

# VALUE ITERATION
gamma = 0.9
g$states$recommended_action = '?'
g$states$value = 0
va_history = NULL
va_changed = TRUE
while(va_changed){
  print('hello ...')
  va_history = cbind(va_history, g$states$value)
  tmp = g$tmat %>% 
    left_join(g$states %>% select(index, value), by = c('state1'= 'index'))
  tmp = tmp %>% 
    group_by(state0, action) %>%
    summarise('value' = sum(prob * (reward + gamma * value))) %>%
    group_by(state0) %>%
    summarise('recommended_action' = best_action(action, value),
              'value' = max(value)) %>%
    ungroup()
  # tmp$recommended_action[tmp$state0 %in% g$states$index[g$states$is_terminal]] = 'multiple'
  tmp$value[tmp$state0 %in% g$states$index[g$states$is_terminal]] = 0
  g$states$recommended_action = tmp$recommended_action
  g$states$value = tmp$value
  va_changed = max(abs(tmp$value - va_history[,ncol(va_history)])) > 0.01
}

g$states$recommended_action = fct_expand(g$states$recommended_action, 'none')
g$states$recommended_action[g$states$is_terminal] = 'none'

from_direction_to_coordinates <- function(x, y, direc){
  if(direc == 'east'){
    return(c(x-0.25, x+0.25, y, y))
  }else if(direc == 'west'){
    return(c(x+0.25, x-0.25, y, y))
  }else if(direc == 'north'){
    return(c(x, x, y+0.25, y-0.25))
  }else if(direc == 'south'){
    return(c(x, x, y-0.25, y+0.25))
  }else{
    return(c(x, x, y, y))
  }
}
hey = t(sapply(1:nrow(g$states), function(i){
  from_direction_to_coordinates(g$states$col[i], g$states$row[i], g$states$recommended_action[i])
})) %>% data.frame()
colnames(hey) <- c('x', 'xend', 'y', 'yend')
hey$category = g$states$recommended_action

ggplot(data=g$states) +
  geom_tile(aes(x = col, y = row, fill = category)) +
  geom_segment(data = hey %>% filter(category %in% c('none', 'multiple') == FALSE), 
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.5, "cm"))) +
  scale_y_reverse() 
# + scale_fill_manual(values = c("abyss"="black", "start"="brown", "grass"="green", "goal"="pink"))
