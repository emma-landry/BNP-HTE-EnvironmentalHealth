###################################################################################
# ---             SIMULATION STUDY:             ---
# ---      function for different  settings     ---
# ---         PRINCIPAL STRATIFICATION          ---
###################################################################################

# library
library(mvtnorm)

###################################################################################

# general functions:

# 2 covariates/confounders
setup_sim_2cov<- function(seed,                        # sed seed for riproducibility
                          eta,sigma_p,                 # mean and variance for the groups/strata of post-treatment 
                          allocation_0,allocation_1,   # cluster-allocation of the units
                          beta_0,beta_1,sigma_y        # parameter for Y-regression (Y=outcome)
                          ){
  
  # set seed for riproducibility
  set.seed(seed)
  
  # covariates
  X=cbind(rbinom(n,1,0.7),rbinom(n,1,0.6))
  
  # treatment
  reg_T=0+0.4*(X[,1])+0.3*(X[,2])
  logit_T=exp(reg_T)/(1+exp(reg_T))
  T=rbinom(n,1,logit_T)
  
  # cluster allocation
  S_dummy=cbind(apply(X,1,prod),
                (X[,1]==0),
                (X[,1]==1 & X[,2]==0))
  S_cl_0=S_dummy%*%allocation_0
  S_cl_1=S_dummy%*%allocation_1
  
  # outcomes simulation
  P=sapply(1:n, function(i) rmvnorm(1,c(eta[S_cl_0[i]],eta[S_cl_1[i]]),
                                    c(sigma_p[S_cl_0[i]],sigma_p[S_cl_1[i]])*diag(2)))
  
  Y=cbind(rnorm(n,beta_0[1]+beta_0[2]*P[1,],exp(sigma_y[1])),
          rnorm(n,beta_1[1]+beta_1[2]*P[2,]+beta_1[3]*P[1,]+beta_1[4]*P[1,]*P[2,],
                exp(sigma_y[1]+sigma_y[2]*P[2,])))
  
  # saving groups and strata allocation
  S_groups=S_cl_1*(S_cl_0+5)
  S_strata=1*(eta[S_cl_1]==eta[S_cl_0])+2*(eta[S_cl_1]>eta[S_cl_0])
  
  return(list(data=list(X=X,T=T,P_0=P[1,],P_1=P[2,],Y_0=Y[,1],Y_1=Y[,2]), 
              par_P=list(eta=eta, sigma_p=sigma_p, 
                          allocation_0=allocation_0, allocation_1=allocation_1),
              par_Y=list(beta_0=beta_0, beta_1=beta_1, sigma_y=sigma_y),
              clusters=list(S_groups=S_groups,S_strata=S_strata)))
}


# 5 covariates/confounders
setup_sim_5cov<- function(seed,                        # sed seed for riproducibility
                          eta,sigma_p,                 # mean and variance for the groups/strata of post-treatment 
                          allocation_0,allocation_1,   # cluster-allocation of the units
                          beta_0,beta_1,sigma_y        # parameter for Y-regression (Y=outcome)
){
  
  # set seed for riproducibility
  set.seed(seed)
  
  # covariates
  # 5 bernulli
  X=cbind(rbinom(n,1,0.4),
          rbinom(n,1,0.6),
          rbinom(n,1,0.3),
          rbinom(n,1,0.5),
          rbinom(n,1,0.2))
  
  # treatment
  reg_T=0+0.4*(X[,1])+0.6*(X[,2])-0.3*(X[,3])+0.2*(X[,4])*(X[,5])
  logit_T=exp(reg_T)/(1+exp(reg_T))
  T=rbinom(n,1,logit_T)
  
  # cluster allocation
  S_dummy=cbind((X[,1]==0 & X[,2]==1),
                (X[,1]==1 | X[,2]==0)*(X[,4]==1))
  S_dummy=cbind(S_dummy,apply(S_dummy,1,sum)==0)
  S_cl_0=S_dummy%*%allocation_0
  S_cl_1=S_dummy%*%allocation_1
  
  # outcomes simulation
  P=sapply(1:n, function(i) rmvnorm(1,c(eta[S_cl_0[i]],eta[S_cl_1[i]]),
                                    c(sigma_p[S_cl_0[i]],sigma_p[S_cl_1[i]])*diag(2)))
  
  Y=cbind(rnorm(n,beta_0[1]+beta_0[2]*P[1,],exp(sigma_y[1])),
          rnorm(n,beta_1[1]+beta_1[2]*P[2,]+beta_1[3]*P[1,]+beta_1[4]*P[1,]*P[2,],
                exp(sigma_y[1]+sigma_y[2]*P[2,])))
  
  # saving groups and strata allocation
  S_groups=S_cl_1*(S_cl_0+10)
  S_strata=1*(eta[S_cl_1]==eta[S_cl_0])+2*(eta[S_cl_1]>eta[S_cl_0])
  
  return(list(data=list(X=X,T=T,P_0=P[1,],P_1=P[2,],Y_0=Y[,1],Y_1=Y[,2]), 
              par_P=list(eta=eta, sigma_p=sigma_p, 
                          allocation_0=allocation_0, allocation_1=allocation_1),
              par_Y=list(beta_0=beta_0, beta_1=beta_1, sigma_y=sigma_y),
              clusters=list(S_groups=S_groups,S_strata=S_strata)))
}
