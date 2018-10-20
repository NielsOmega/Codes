simulate_bernoulli=function(n,p,o,m){
  
  set.seed(9)
  b=matrix(0,n,m)
  for (i in 1:m){
  
    b[,i]=t(t(rbinom(n,1,p)))
    b[b==0]=-1
  
  }
  b=b*o # wager modification
  
  return(b)
  
}