# Setting:
#  we are trying to learn how to navigate in a [0,1]x[0,1] grid
#  where the exit is in a square of the grid [0.75,0.85]x[0.75,0.85]
#  and there is a wall at x=0.5 and from y=0.5 to 1.0
# Constraint: no discretization of the state space
# We are using linear function approximation to solve this problem

# SETUP =============================================================

# libraries
library(tidyverse)

# grid parameters
# can be much more complicated with obstacles and everything
mygridenv <- list(
  "endstate" = list(
    xmin=0.75, 
    xmax=0.85,
    ymin=0.75, 
    ymax=0.85
  ),
  "obstacle" = list(
    x=0.5,
    y0=0.5,
    y1=1.00
  ))

# evaluate point of intersection
point_of_intersection <- function(x0, y0, x1, y1, obstacle){
  # obstacle is a list 
  if(obstacle$x>min(x0, x1) & obstacle$x<max(x0,x1)){
    a = (y1-y0)/(x1-x0)
    b = y1-x1*a
    ymatch = a*obstacle$x+b
    if(obstacle$y0<ymatch & obstacle$y1>ymatch){
      return(c(obstacle$x, ymatch))
    }else{
      return(NULL)
    }
  }else{
    return(NULL)
  }
}


# simulate a scenario
simulateContinuousGridGame <- function(tmax, gridenv, delta=1/10){
  pos <- runif(n = 2)
  df <- tibble(index=1:tmax, x=NA, y=NA, angle=NA, reward=NA)
  if(pos[1]>gridenv$endstate$xmin & pos[1]<gridenv$endstate$xmax 
     & pos[2]>gridenv$endstate$ymin & pos[2]<gridenv$endstate$ymax){
    df$x[1] <- pos[1]
    df$y[1] <- pos[2]
    df$angle[1] <- runif(n=1,min=0,max=360)/360*2*pi
    df$reward[1] <- 100
    return(df[1,])
  }
  not_done <- TRUE
  tindex <- 1
  while(not_done){
    # take random action
    angle <- runif(n=1,min=0,max=360)/360*2*pi
    # store info
    df$x[tindex] <- pos[1]
    df$y[tindex] <- pos[2]
    df$angle[tindex] <- angle
    # compute next state
    pos_ <- pos
    dx <- cos(angle)*delta
    dy <- sin(angle)*delta
    pos_[1] <- max(min(pos[1] + dx, 1), 0)
    pos_[2] <- max(min(pos[2] + dy, 1), 0)
    point_intersection <- point_of_intersection(x0=pos[1], y0=pos[2], x1=pos[1]+dx, y1=pos[2]+dy, obstacle = gridenv$obstacle)
    penalty <- 0
    if(!is.null(point_intersection)){
      pos_[1] <- point_intersection[1]
      pos_[2] <- point_intersection[2]
      penalty <- (-30)
    }
    pos = pos_
    if(pos[1]>gridenv$endstate$xmin & pos[1]<gridenv$endstate$xmax 
       & pos[2]>gridenv$endstate$ymin & pos[2]<gridenv$endstate$ymax){
      not_done <- FALSE
      df$reward[tindex] <- +100
    }else{
      df$reward[tindex] <- -1 + penalty
    }
    if(not_done){
      if(tindex>=tmax){
        not_done <- FALSE
      }else{
        tindex <- tindex+1 
      }
    }
  }
  return(df[1:tindex,])
}

# SIMULATE DATA ================================================

# simulate a bunch a scenarios
gameSim <- list()
for(i in 1:200){
  gameSim[[i]] <- simulateContinuousGridGame(tmax=1000, gridenv = mygridenv, delta=1/10) %>% 
    mutate(simu_index=i,
           next_x=c(dplyr::lead(x)[-length(x)], x[length(x)]),
           next_y=c(dplyr::lead(y)[-length(y)], y[length(y)]))
}
gameSim <- bind_rows(gameSim)
gameSim <- gameSim %>% mutate(is_terminal = (reward>10))

# check how many game reach the goal 
# <!> if there is not many cases that reach the goal
#     the algorithm will NOT learn how to get out of it
sum(gameSim$is_terminal)

# check for missing values and overall distribution
summary(gameSim)

# BUILD NON-LINEAR FEATURES ====================================================

# construct basis of functions
# functions of (x,y)
N <- 5
x_seq <- seq(0,1,length.out=N)
y_seq <- x_seq
angle_seq <- seq(0,2*pi,length.out=5)[-1]
idx <- 0
for(x in x_seq){
  for(y in y_seq){
      gameSim[paste0("feature_xy_", idx)] = exp(- ((gameSim$x-x)^2 + (gameSim$y-y)^2))
      gameSim[paste0("feature_next_xy_", idx)] = exp(- ((gameSim$next_x-x)^2 + (gameSim$next_y-y)^2))
      idx = idx+1
    }
}
# functions of angle taken
idx <- 0
for(angle in angle_seq){
  gameSim[paste0("feature_angle_", idx)] = exp(-(gameSim$angle-angle)^2)
  idx <- idx+1
}

# LEARNING WITH FUNCTION APPROXIMATION =========================================

# initialize with reward as target
# cross the features of (x,y) and angle
v0 <- lm(formula = reward ~ (feature_xy_0+feature_xy_1+feature_xy_2+feature_xy_3)*(feature_angle_0+feature_angle_1+feature_angle_2+feature_angle_3), 
         data = gameSim %>% select(reward, starts_with(match="feature_xy"), starts_with(match="feature_angle")))

# store coefficients to update with learning rate
running_theta <- v0$coefficients

# store models
v = list()
v[[1]] <- v0

# learning rate \alpha
alpha <- 0.2

# angles to check in evaluation of max_a Q(s,a) // simplifies implementation, but could be replaced by an optim()
angles_to_check <- seq(from=0, to=2*pi, length.out=10)

# learning algorithm loop
pb <- txtProgressBar(min=2, max=100, initial=2)
for(iter in 2:100){
  
  # prepare data
  tmp <- gameSim %>% 
    select(reward, starts_with(match="feature_next_xy")) %>%
    rename_with(.fn = function(nm){sub("_next", "", nm)}, .cols = -reward)
  
  # compute max_a Q(s,a)
  next_qsa_ <- sapply(X=angles_to_check, FUN = function(a){
    idx <- 0
    for(angle in angle_seq){
      tmp[paste0("feature_angle_", idx)] = exp(-(a-angle)^2)
      idx <- idx+1
    }
    pred <- running_theta[1]
    for(i in 2:length(running_theta)){
      if(!is.na(running_theta[i])){
        if(grepl(pattern="\\:", x=names(running_theta[i]))){
          nam <- ((names(running_theta)[i]) %>% str_split(pattern = ":"))[[1]]
          pred <- pred + running_theta[i]*tmp[,nam[1]]*tmp[,nam[2]]
        }else{
          pred <- pred + running_theta[i]*tmp[,names(running_theta)[i]]
        }
      }
    }
    return(pred[[1]])
  })
  gameSim$next_qsa <- apply(X = next_qsa_, MARGIN = 1, FUN = max, na.rm=TRUE)
  gameSim$next_qsa[gameSim$is_terminal] <- 0
  
  # update coefficients
  # . compute with linear model function
  v[[iter]] <- lm(formula = reward + next_qsa ~ (feature_xy_0+feature_xy_1+feature_xy_2+feature_xy_3)*(feature_angle_0+feature_angle_1+feature_angle_2+feature_angle_3), 
                  data = gameSim %>% select(reward, next_qsa, starts_with(match="feature_xy"), starts_with(match="feature_angle")))
  # . update with learning rate rule
  running_theta <- (1-alpha)*running_theta + alpha*v[[iter]]$coefficients
  

  setTxtProgressBar(pb = pb, value = iter)
  
}

# DISPLAY OPTIMAL POLICY APPROXIMATED ==============================

# collect best_angle from last iteration
gameSim$best_angle <- angles_to_check[apply(next_qsa_, MARGIN = 1, FUN = which.max)]

# discretizing the results using stat_summary_2d
ggplot(data = gameSim) +
  aes(x=x, y=y, z=best_angle/pi*180) +
  scale_color_viridis_c() +
  stat_summary_2d(bins = 50, fun = mean) +
  geom_segment(aes(x=mygridenv$obstacle$x, y=mygridenv$obstacle$y0,
                   xend=mygridenv$obstacle$x, yend=mygridenv$obstacle$y1)) +
  scale_fill_viridis_c() +
  labs(fill = "Angle recommended") +
  theme(legend.position = "top") +
  coord_equal()

# what is the point of solving it in a continuous way ?
# a) in practice, the data we have is often continuous data
# b) discretization choice before resolution is a blind discretization
#    and therefore can make the solutions found highly suboptimal
# c) you can always go back to discretization after having solved it
