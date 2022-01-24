# define zones
w0 = 0.3
w1 = 0.7
w2 = 0.3
env_zone = list('x' = 0, 'xend' = 1, 'y' = 0, 'yend' = 1)
start_point_zone = list('x' = 0, 'xend' = w0, 'y' = 0, 'yend' = w2)
abyss_zone = list('x' = w0, 'xend' = w1, 'y' = 0, 'yend' = w2)
goal_zone = list('x' = w1, 'xend' = 1, 'y' = 0, 'yend' = w2)

# get next coordinate
# angle in [0, 2*pi]
get_next_coordinate <- function(x, y, angle){
  c(x+cos(angle), y+sin(angle))
}

is_in_goal <- function(x, y){
  in_goal <- (x >= goal_zone$x) & (x <= goal_zone$xend) & (y >= goal_zone$y) & (y <= goal_zone$yend)
  return(in_goal)  
}

is_in_abyss <- function(x, y){
  in_abyss <- (x >= abyss_zone$x) & (x <= abyss_zone$xend) & (y >= abyss_zone$y) & (y <= abyss_zone$yend)
  return(in_abyss)  
}

get_reward <- function(x, y){
  in_goal <- (x >= goal_zone$x) & (x <= goal_zone$xend) & (y >= goal_zone$y) & (y <= goal_zone$yend)
  in_abyss <- (x >= abyss_zone$x) & (x <= abyss_zone$xend) & (y >= abyss_zone$y) & (y <= abyss_zone$yend)
  v = rep(-1, length(x))
  v[in_goal] = 20
  v[in_abyss] = -20
  return(v)
}

library(mgcv)
library(randomForest)
library(tidyverse)

nb = 1000
ww = 0.05
df = tibble(x = runif(n=nb), y = runif(n=nb), a = runif(n=nb, min=0, max=2*pi)) %>%
  mutate(
    'xy_in_goal' = is_in_goal(x, y),
    'xy_terminal' = is_in_goal(x, y) | is_in_abyss(x, y),
    'x.next' = pmin(pmax(x+ww*cos(a), 0), 1),
    'y.next' = pmin(pmax(y+ww*sin(a), 0), 1),
    'reward' = get_reward(x.next, y.next),
    'qsa' = reward)
mo = randomForest(formula = qsa ~ x + y + a, data = df %>% filter(!xy_terminal), ntree = 100, mtry = 3)
# predict(mo, tibble("x"=1, "y"=1, "a"=0))
not_done = TRUE
while(not_done){
  v <- sapply(1:nrow(df), function(i){
    max_qsa = max(predict(mo, tibble("x"=df$x[i], "y"=df$y[i], "a"=seq(0, 2*pi, length.out=100))))
    df$reward[i] + 0.9 * max_qsa
  })
  print(max(abs(df$qsa - v)))
  df$qsa <- v
  mo = randomForest(formula = qsa ~ x + y + a, data = df %>% filter(!xy_terminal), ntree = 100, mtry = 3)
}

dff = expand.grid(x = seq(0, 1, length.out = 20),
                  y = seq(0, 1, length.out = 20)) %>% tibble() 
action_vector = seq(0, 2*pi, length.out=100)
v <- sapply(1:nrow(dff), function(i){
  which.max(predict(mo, tibble("x"=dff$x[i], "y"=dff$y[i], "a"=action_vector)))
})
dff <- dff %>%
  mutate(best_action = action_vector[v]) %>%
  mutate(reward = get_reward(x, y)) %>%
  mutate(xy_terminal = is_in_goal(x, y) | is_in_abyss(x, y))
library(plotly)
ggplot(data=dff) +
  geom_point(aes(x=x, y=y, col=reward)) + #col=360/(2*pi)*best_action)) + 
  scale_color_viridis_c()
www = 0.05
ggplot(data=dff %>% filter(!xy_terminal)) +
  geom_point(aes(x=x, y=y, col=360/(2*pi)*best_action)) +
  geom_segment(aes(x=x, y=y, 
                   xend=x+www*cos(best_action),
                   yend=y+www*sin(best_action)),
               arrow = arrow(length = unit(0.5, "cm"))) +
  scale_color_viridis_c()

  