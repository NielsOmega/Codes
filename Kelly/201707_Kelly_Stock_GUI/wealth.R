wealth=function(W0,f,m,n,ret_sim_discrete){
  
W_T=matrix(nrow=m, ncol=n)  
  
  for(i in 1:m){
    W_T[i,]=W0*(1+f[1,i]*ret_sim_discrete[1,]+f[2,i]*ret_sim_discrete[2,])
  }
  W_T[W_T<0]=0 
  
  return(W_T)
  
}

