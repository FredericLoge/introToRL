# EXACT CONFIDENCE INTERVAL =======================================================

# confidence interval
get_ci <- function(low, high, q, v){
  s = which(q < low)
  if(length(s) == 0){
    s = 1 
  }else{
    s = dplyr::last(s)
  }
  e = which(q > high)
  if(length(e) == 0){
    e = length(q)
  }else{
    e = dplyr::first(e)
  }
  return(v[c(s,e)])
}

#' @title Compute confidence interval
#' @param n number of trials
#' @param s number of successes
#' @param alpha pct controlling CI width
get_CI <- function(n, s, alpha){
  
  # grid of probability values
  p_vec = seq(0, 1, length.out = 1000)
  
  # compute P(S <= s | Binomial(n,p)), for p \in p_vec
  y_vec = dbinom(x = s, size = n, prob = p_vec)
  
  # normalize this probability
  y_vec_norm = cumsum(y_vec)/sum(y_vec)
  
  # compute all possible CIs
  ci = list(
    'centered' = get_ci(low = alpha/2, high = 1-alpha/2, v = p_vec, q = y_vec_norm),
    'left' = get_ci(low = 0.00, high = 1-alpha, v = p_vec, q = y_vec_norm),
    'right' = get_ci(low = alpha, high = 1, v = p_vec, q = y_vec_norm)
  )
  
  # check that actual value is contained
  cond_a = sapply(ci, function(x){ s/n >= x[1] & s/n <= x[2] })
  
  # check range of each confidence interval
  ci_range = sapply(ci, function(x){ x[2]-x[1] })
  
  # auto selection
  ci_to_get = names(which.min(ci_range[cond_a]))
  ci[['auto']] = ci[[ci_to_get]]
  
  return(ci)
  
}
