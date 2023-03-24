minNbCoins <- function(qty, 
                       returnLast=TRUE, 
                       coinsAvailable=c(1, 2, 5, 10, 20, 50)){
  PENALTY = 10000
  dp = PENALTY + numeric(length=qty)
  dp[coinsAvailable] = 1
  if(!1 %in% coinsAvailable){
    dp[1] = PENALTY
  }
  for(i in 2:length(dp)){
    if(!i %in% coinsAvailable){
      dpsub = sapply(X = 1:floor((i+1)/2), FUN = function(j){
        dp[j] + dp[i-j]
      })
      dp[i] = min(dpsub)
    }
  }
  if(returnLast){
    return(dp[length(dp)])
  }else{
    return(dp)
  }
}

ca = c(1, 2, 5, 10, 20, 50)
v = minNbCoins(qty = 100, returnLast=FALSE, coinsAvailable=ca)
plot(x=1:100, y=v, type='o', pch='x')
barplot(table(v))

ca = c(1, 3, 10, 25, 32)
v = minNbCoins(qty = 100, returnLast=FALSE, coinsAvailable=ca)
plot(x=1:100, y=v, type='o', pch='x')
barplot(table(v))
