simulate_scenario1_DA <- function(P = 7, n = 300, seed) {
  cov <- list()
  for(i in 1:P){
    cov[[i]] <- rnorm(n,0,1)
  }
  Xpred <- do.call(cbind, cov)
  
  reg1 <- ifelse(Xpred[,1]< 0, 1, -1 )
  reg2 <- ifelse(Xpred[,2]< 0, -1, 1 )
  
  Y_trt <- rbinom(n, 1, pnorm(0.5+reg1+reg2-0.5*abs(Xpred[,3]-1)+1.5*Xpred[,4]*Xpred[,5])) # exposure model
  
  M_err <- rnorm(n, 0, 0.1)
  M_1 <- 0.5*reg1+0.5*reg2+1*2+1*Xpred[,3]+ 1+ 1.5*Xpred[,4] - exp(0.3*Xpred[,5]) + 1*1*Xpred[,5] + M_err
  M_0 <- 0.5*reg1+0.5*reg2+(0)*2+1*Xpred[,3]+1 + 1.5*Xpred[,4] - exp(0.3*Xpred[,5]) + 1*(0)*Xpred[,5] + M_err
  M_out <- ifelse(Y_trt==1, M_1, M_0)
  
  S <- M_1-M_0
  
  Y_err <- rnorm(n, 0, 0.3)
  Y_1 <- 1*reg1+1.5*reg2-(S)^2*1+2*abs(Xpred[,3]+1)   + 2*Xpred[,4]+ exp(0.5*Xpred[,5])-0.5*abs(Xpred[,6]) - 1*abs(Xpred[,7]+1)+Y_err
  Y_0 <- 1*reg1+1.5*reg2-(S)^2*(0)+2*abs(Xpred[,3]+1) + 2*Xpred[,4]+ exp(0.5*Xpred[,5])-0.5*abs(Xpred[,6]) - 1*abs(Xpred[,7]+1)+Y_err
  Y_out <- ifelse(Y_trt==1, Y_1, Y_0)
  
  return(list(Y_out = Y_out, Y_1 = Y_1, Y_0 = Y_0,
              M_out = M_out, M_1 = M_1, M_0 = M_0,
              Y_trt = Y_trt,
              Xpred = Xpred,
              S = S))
} 

simulate_scenario1_DA_alt <- function(P = 5, n = 300, seed) {
  cov <- list()
  for(i in 1:2){
    cov[[i]] <- runif(n,0,1)
  }
  cov[[3]] <- rnorm(n, 1, 1) 
  cov[[4]] <- rnorm(n, -1, 2) 
  cov[[5]] <- rnorm(n, 1, 0.5) 
  
  Xpred <- do.call(cbind, cov)
  Xcut <- lapply(1:dim(Xpred)[2], function(t) sort(unique(Xpred[,t]))) # e.g. unique values of predictors
  
  
  MU_func <- -1+Xpred[,1]-Xpred[,2]+1*Xpred[,3]+Xpred[,4]+Xpred[,5]
  
  Y_trt <- rbinom(n, 1, 0.8*pnorm(0.5*MU_func / (0.1*(2-Xpred[,1]-Xpred[,2])+0.25))+0.1*(Xpred[,1]+Xpred[,2])) # exposure model
  
  M_err <- rnorm(n,0,0.1)
  M_1 <- 1.5+0.9*1+1*(Xpred[,1])+1.5*Xpred[,2]+1*abs(Xpred[,3]+1) + 1*1*Xpred[,4] - 0.5*Xpred[,5]+M_err
  M_0 <- 1.5+0.9*0+1*(Xpred[,1])+1.5*Xpred[,2]+1*abs(Xpred[,3]+1) + 0*1*Xpred[,4] - 0.5*Xpred[,5]+M_err
  M_out <- ifelse(Y_trt==1, M_1, M_0)
  
  S <- M_1-M_0
  
  Y_err <- rnorm(n, 0, 0.3)
  Y_1 <- MU_func-1.5*1*(M_1-M_0)+1.5*Xpred[,3]+Y_err
  Y_0 <- MU_func-1.5*0*(M_1-M_0)+1.5*Xpred[,3]+Y_err
  
  Y_out <- ifelse(Y_trt==1, Y_1, Y_0)
  
  return(list(Y_out = Y_out, Y_1 = Y_1, Y_0 = Y_0,
              M_out = M_out, M_1 = M_1, M_0 = M_0,
              Y_trt = Y_trt,
              Xpred = Xpred,
              S = S))
}
