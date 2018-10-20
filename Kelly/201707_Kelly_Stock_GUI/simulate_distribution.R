simulate_distribution=function(mu,rf,n,sim){
  
  # working directory
  # setwd("Finance/201701 Kelly/201707 Kelly One-dimensional GUI")
  load("ret.RData")
  
  
  
  T=251
  if(sim=="gaussian"){
    set.seed(8)
    ret_sim=rnorm(n, mean = 0, sd = sqrt(T)*sd(ret))
  }  else if(sim=="student"){
    set.seed(1)
    theta=fitdistr(ret, "t")
    ret_sim=sqrt(T)*theta$estimate[2]*rt(n, theta$estimate[3])
  } else if(sim=="stable"){
    #load("Stable.RData")
    #theta=Estim(EstimMethod=c("ML"),ret,theta0=c(1.7,0,0.1,0.1)) #"ML", "GMM", "Cgmm","Kout" # crap
    set.seed(1)
    ret_sim=rstable(n, 1.7, 0, 0.0084*(T^(1/1.7)), 0,pm=2)

  }
  
  ret_sim_exp_raw=exp(ret_sim)-1
  ret_sim_exp=ret_sim_exp_raw[! ret_sim_exp_raw>10]
  ret_sim_discrete=rbind(mu+ret_sim_exp-mean(ret_sim_exp),rep(rf,n))
  
  plot(density(ret_sim_discrete[1,]),xlim=c(0,5))
  
  
  
  return(ret_sim_discrete)
  
}