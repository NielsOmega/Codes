wealth_bernoulli=function(W0,b,n,m,f){
  # WEALTH_BERNOULLI Wealth trajectories for repeated bernoulli
  
  W=matrix(0,n+1,m)
  for (j in 1:m){
    W[1,j]=W0
    for (i in 1:n){
      W[i+1,j]=W[i,j]*(1+b[i,j]*f)
    }
  }
  W[W<=0]=0
  
  return(W)
  
}