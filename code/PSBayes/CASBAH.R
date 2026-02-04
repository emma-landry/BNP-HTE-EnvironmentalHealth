###################################################################################
# ---               Gibbs sampler:              ---
# ---  CONFONDER-DEPENDENT SHARED ATOMS MODEL   ---
# ---        WITHOUT Fasano e Durante           ---
###################################################################################

#library
library(mvtnorm)
library(CholWishart)
library(parallel)
library(truncnorm)
library(invgamma)
library(BNPmix)

###################################################################################

Gibbs_CASDMM<-function(c, sim, scenario1 = F){
  
  # set seed for riproducibility
  set.seed(c)
  
  # ------   prearing variables   ------
  
  # main variables
  if (!scenario1) {
    matrix_X=cbind(rep(1,n),sim[[c]]$data$X)
    T_var=sim[[c]]$data$T
    P_obs=T_var*sim[[c]]$data$P_1+(1-T_var)*sim[[c]]$data$P_0
    Y_obs=T_var*sim[[c]]$data$Y_1+(1-T_var)*sim[[c]]$data$Y_0
  } else {
    matrix_X=cbind(rep(1,n),sim[[c]]$Xpred)
    T_var=sim[[c]]$Y_trt
    P_obs=T_var*sim[[c]]$M_1+(1-T_var)*sim[[c]]$M_0
    Y_obs=T_var*sim[[c]]$Y_1+(1-T_var)*sim[[c]]$Y_0
  }
  
  
  # number covariates (X)
  n_X=dim(matrix_X)[2]
  
  # dividing observation in the treatment levels
  # level t=1
  T1=which(T_var==1)
  n1=length(T1)
  P_obs_1=P_obs[T1]
  Y_obs_1=Y_obs[T1]
  # level t=0
  T0=which(T_var==0)
  n0=length(T0)
  P_obs_0=P_obs[T0]
  Y_obs_0=Y_obs[T0]
  
  # preparing vector for P imputation
  P_mis=rep(mean(P_obs),n)
  
  # preparing vector for Y imputation
  Y_0_imp=rep(mean(Y_obs_0),n)
  Y_1_imp=rep(mean(Y_obs_1),n)
  
  # ------   hyperparameters   -----
  # for P-model
  p_beta=c(-0.5,20)                 # mean and variance of normal distribution for beta param.
  p_sigma=c(2,0.5)              # shape parameters of inv-gamma for sigma param.
  p_eta=c(0,20)                  # mean and variance of normal distribution for eta param.
  # for Y-model
  p_theta=c(0,10)                # mean and variance of normal distribution for theta param.
  p_lambda=c(0,2,0,2)           # mean and variance of normal distribution for lambda param.
  
  # ------   initialitation   -----
  # parameters
  beta_0=rep(0,n_X*(n_cluster-1))
  beta_1=rep(0,n_X*(n_cluster-1))
  sigma=rep(1,n_cluster)
  eta=seq(-3,-3+0.5*(n_cluster-1),0.5)
  theta_0=rep(0.1,2)
  theta_1=rep(0.1,4)
  lambda=rep(0.1,2)
  # cluster allocatio variables 
  xi_0=sample(1:n_cluster,n0,replace=TRUE)
  xi_1=sample(1:n_cluster,n1,replace=TRUE)
  
  
  # ------   useful quantities   ------   
  # number of units in each cluster
  units_for_clusters_0=table(xi_0)
  units_for_clusters_1=table(xi_1)
  # eta corresponding to each unit
  eta_sogg_0=eta[xi_0]
  eta_sogg_1=eta[xi_1]
  # latent variable for data augmentation in probit regression 
  Z_0=rep(list(rep(NA,n_cluster)),n0)
  Z_1=rep(list(rep(NA,n_cluster)),n1)
  # acceptation rate for the metropolis proposal step (variance Y-model)
  acc_0=0
  acc_1=0
  
  # ------   useful functions   -----
  
  # probit stick-breaking weights 
  omega=function(X,beta){
    L=n_cluster-1
    alpha=c(sapply(1:L, function(l) pnorm(beta[(l*n_X-(n_X-1)):(l*n_X)]%*%X)),1)
    return(c(alpha[1],sapply(2:(L+1), function(l) alpha[l]*prod(1-alpha[1:(l-1)]))))
  }
  
  # point estimate parition: keeping all clusters
  partition_all_cluster<-function(xi_0,xi_1){
    grid_couple=expand.grid(1:n_cluster,1:n_cluster)
    
    #cartesian product of cluster allocation
    part=sapply(1:n, function(i)
      sapply(1:(R-R_burnin), function(r) 
        which(grid_couple$Var1==xi_0[i,r] & grid_couple$Var2==xi_1[i,r])))
    
    #point estimation
    WG=partition.BNPdens(list(clust=part),dist = "VI")$partitions[1,]
    tab=cbind(WG)
    dimnames(tab)=NULL
    return(tab)
  }
  
  # point estimate parition: using strata definitions
  partition_strata_cluster<-function(xi_0,xi_1, eta){
    #stratat allocation
    part=sapply(1:n, function(i)
      sapply(1:(R-R_burnin), function(r) 
        ifelse(xi_0[i,r]==xi_1[i,r], 0, 
               1*(eta[xi_1[i,r],r]>eta[xi_0[i,r],r])+
                 (-1)*(eta[xi_0[i,r],r]>eta[xi_1[i,r],r])
        )))
    
    #point estimation
    WG=partition.BNPdens(list(clust=part),dist = "VI")$partitions[1,]
    tab=cbind(WG)
    dimnames(tab)=NULL
    return(tab)
    
  }
  
  # ------   saving informations   -----
  
  # empty matrix where save all the informations for each iteration
  post_eta=matrix(NA,ncol=R-R_burnin,nrow=n_cluster)
  post_var=matrix(NA,ncol=R-R_burnin,nrow=n_cluster)
  cluster_allocation_0=matrix(NA,ncol=R-R_burnin,nrow=n)     
  cluster_allocation_1=matrix(NA,ncol=R-R_burnin,nrow=n)     
  post_beta=matrix(NA,ncol=R-R_burnin, nrow=length(beta_0)+length(beta_1))
  post_theta=matrix(NA,ncol=R-R_burnin,nrow=6)
  post_lambda=matrix(NA,ncol=R-R_burnin, nrow=2)
  
  post_P_0=matrix(NA,ncol=R-R_burnin,nrow=n)
  post_P_1=matrix(NA,ncol=R-R_burnin,nrow=n)
  post_Y_0_imp=matrix(NA,ncol=R-R_burnin,nrow=n)
  post_Y_1_imp=matrix(NA,ncol=R-R_burnin,nrow=n)
  
  # -----   updating parameters and variables at each itearation   -----
  
  for (r in 1:R){
    if (r%%10==0) print(paste0(r,"/",R," iterations"))
    # ----------- Cluster Specific Parameters  ------------
    
    # -----   ETA: mean of normal distributions (atoms)   -----
    # in common between the two treatment level t=0 and t=1
    for(l in 1:n_cluster){                                      
      v_eta_inv=(units_for_clusters_0[l]+units_for_clusters_1[l])/sigma[l]+1/p_eta[2]
      m_eta=(sum(P_obs_0[xi_0==l])+sum(P_obs_1[xi_1==l]))/sigma[l]+p_eta[1]/p_eta[2]
      eta[l]=rnorm(1,mean=1/v_eta_inv*(m_eta),
                   sd=sqrt(1/v_eta_inv))
    }
    
    eta_sogg_0=eta[xi_0]
    eta_sogg_1=eta[xi_1]
    
    # -----   SIGMA: variance of normal distributions (atoms)   -----
    # in common between the two treatment level t=0 and t=1
    sigma=sapply(1:n_cluster, function(l) 
      rinvgamma(1,p_sigma[1]+(sum(xi_0==l)+sum(xi_1==l))/2,
                p_sigma[2]+(sum((P_obs_0[xi_0==l]-eta[l])^2)+
                              sum((P_obs_1[xi_1==l]-eta[l])^2))/2))
    
    # ----------- Cluster Allocation  ------------
    
    # -----   OMEGA: weights treatment-specific   -----
    # omega_0 = omega for treatment level 0
    omega_0=sapply(1:n0, function(i) omega(X=matrix_X[T0[i],],beta=beta_0))
    # omega_1 = omega for treatment level 1
    omega_1=sapply(1:n1, function(i) omega(X=matrix_X[T1[i],],beta=beta_1))
    
    # -----   LATENT VARIABLE for cluster allocation   -----
    # xi_0 = latent variable for treatment level 0
    dmn_0=sapply(1:n0, function(i) sapply(1:n_cluster, function(l) dnorm(P_obs_0[i], eta[l], sqrt(sigma[l]), log=TRUE)))+log(omega_0)
    dmn_0[which(is.nan(dmn_0))]=-100
    xi=sapply(1:n0, function(i) rmultinom(1,1,exp(dmn_0[,i])))
    
    xi_0=sapply(1:n0, function(i) xi[,i]%*%(1:n_cluster))
    units_for_clusters_0=apply(xi, 1, sum)
    
    # xi_1 = latent variable for treatment level 1
    dmn_1=sapply(1:n1, function(i) sapply(1:n_cluster, function(l) dnorm(P_obs_1[i], eta[l], sqrt(sigma[l]), log=TRUE)))+log(omega_1)
    dmn_1[which(is.nan(dmn_1))]=-100
    xi=sapply(1:n1, function(i) rmultinom(1,1,exp(dmn_1[,i])))
    
    xi_1=sapply(1:n1, function(i) xi[,i]%*%(1:n_cluster))
    units_for_clusters_1=apply(xi, 1, sum)
    
    # ----------- Augmentation Scheme  ------------
    
    # -----   LATENT VARIABLE: Z for probit regression   -----
    # Z_0 = latent variable for treatment level 0
    # building intermediate values
    pesi_0=t(omega_0)
    mu_z=cbind((pesi_0[,1]),(pesi_0[,2]/(1-pesi_0[,1])))
    if (n_cluster>3){
      mu_z=cbind(mu_z,sapply(3:(n_cluster-1), function(l) (pesi_0[,l]/(1-apply(pesi_0[,1:(l-1)],1,sum)))))
    }
    mu_z[which(is.nan(mu_z))]=1
    mu_z[which(mu_z>1)]=1
    mu_z=mu_z-9.9e-15*(mu_z>(1-1e-16))
    # updating Z_0:
    for (i in 1:n0){
      for (l in 1:(min(xi_0[i],n_cluster-1))) {
        if(l>1){
          if (l<xi_0[i]){
            Z_0[[i]][l]=rtruncnorm(1,b=0,mean=qnorm(mu_z[i,l]))
          }else{
            Z_0[[i]][l]=rtruncnorm(1,a=0,mean=qnorm(mu_z[i,l]))
          }
        }else{
          if (l<xi_0[i]){
            Z_0[[i]][l]=rtruncnorm(1,b=0,mean=qnorm(mu_z[i,l]))
          }else{
            Z_0[[i]][l]=rtruncnorm(1,a=0,mean=qnorm(mu_z[i,l]))
          }
        }
      }
    }
    
    # Z_1 = latent variable for treatment level 1
    # building intermediate values
    pesi_1=t(omega_1)
    mu_z=cbind((pesi_1[,1]),(pesi_1[,2]/(1-pesi_1[,1])))
    if (n_cluster>3){
      mu_z=cbind(mu_z,sapply(3:(n_cluster-1), function(l) (pesi_1[,l]/(1-apply(pesi_1[,1:(l-1)],1,sum)))))
    }
    mu_z[which(is.nan(mu_z))]=1
    mu_z[which(mu_z>1)]=1
    mu_z=mu_z-9.9e-15*(mu_z>(1-1e-16))
    # updating Z_1:
    for (i in 1:n1){
      for (l in 1:(min(xi_1[i],n_cluster-1))) {
        if(l>1){
          if (l<xi_1[i]){
            Z_1[[i]][l]=rtruncnorm(1,b=0,mean=qnorm(mu_z[i,l]))
          }else{
            Z_1[[i]][l]=rtruncnorm(1,a=0,mean=qnorm(mu_z[i,l]))
          }
        }else{
          if (l<xi_1[i]){
            Z_1[[i]][l]=rtruncnorm(1,b=0,mean=qnorm(mu_z[i,l]))
          }else{
            Z_1[[i]][l]=rtruncnorm(1,a=0,mean=qnorm(mu_z[i,l]))
          }
        }
      }
    }
    
    # ----------- Confounder-Dependent Weights  ------------
    
    # -----   BETA: parameters for probit regression   -----
    # beta_0 = beta for treatment level 0
    groups=which(units_for_clusters_0!=0)
    if (max(groups)==n_cluster)
      groups=groups[-length(groups)]
    # updating beta_0 for cluster WITH allocated units
    for (l in groups){
      val=which(xi_0>=l)
      z_tilde=unlist(sapply(val, function(i) Z_0[[i]][l]))
      x_tilde=matrix(matrix_X[T0[val],],ncol=n_X)
      V=solve(diag(n_X)/p_beta[2]+t(x_tilde)%*%x_tilde)
      beta_0[(l*n_X-(n_X-1)):(l*n_X)]=rmvnorm(1,V%*%(1/p_beta[2]*(diag(n_X)%*%rep(p_beta[1],n_X))+t(x_tilde)%*%z_tilde),V)[,1:n_X]
    }
    # updating beta_0 for cluster WITHOUT allocated units
    empty_clusters=which(units_for_clusters_0==0)
    if (length(empty_clusters)>0){
      if (max(empty_clusters)==n_cluster)
        empty_clusters=empty_clusters[-length(empty_clusters)]
    }
    for (l in empty_clusters){
      beta_0[(l*n_X-(n_X-1)):(l*n_X)]=rmvnorm(1,rep(p_beta[1],n_X),diag(n_X)/p_beta[2])[,1:n_X]
    }
    
    # beta_1 = beta for treatment level 1
    groups=which(units_for_clusters_1!=0)
    if (max(groups)==n_cluster)
      groups=groups[-length(groups)]
    # updating beta_1 for cluster WITH allocated units
    for (l in groups){
      val=which(xi_1>=l)
      z_tilde=unlist(sapply(val, function(i) Z_1[[i]][l]))
      x_tilde=matrix(matrix_X[T1[val],],ncol=n_X)
      V=solve(diag(n_X)/p_beta[2]+t(x_tilde)%*%x_tilde)
      beta_1[(l*n_X-(n_X-1)):(l*n_X)]=rmvnorm(1,V%*%(1/p_beta[2]*(diag(n_X)%*%rep(p_beta[1],n_X))+t(x_tilde)%*%z_tilde),V)[,1:n_X]
    }
    # updating beta_1 for cluster WITHOUT allocated units
    empty_clusters=which(units_for_clusters_1==0)
    if (length(empty_clusters)>0){
      if (max(empty_clusters)==n_cluster)
        empty_clusters=empty_clusters[-length(empty_clusters)]
    }
    for (l in empty_clusters){
      beta_1[(l*n_X-(n_X-1)):(l*n_X)]=rmvnorm(1,rep(p_beta[1],n_X),diag(n_X)/p_beta[2])[,1:n_X]
    }
    
    # ----------- imputing post-treatment variables ----------- 
    
    # -----   P_MIS: post-treatent missing: P(1-t)   -----
    # level t=0 observed --> level T=1 missing
    safe_multinom <- function(p, eps = 1e-12) {
      p[!is.finite(p)] <- 0
      p[p < eps] <- eps
      p / sum(p)
    }
    
    om_0=sapply(T0, function(i) omega(X=matrix_X[i,],beta=beta_1))
    om_0[which(is.nan(om_0))]=0
    p0=sapply(1:n0, function(i) (1:n_cluster)%*%(rmultinom(1,1, safe_multinom(om_0[,i]))))
    P_mis[T0]=rnorm(T0,eta[p0],sqrt(sigma[p0]))
    # level t=1 observed --> level T=0 missing
    om_1=sapply(T1, function(i) omega(X=matrix_X[i,],beta=beta_0))
    om_1[which(is.nan(om_1))]=0
    p1=sapply(1:n1, function(i) (1:n_cluster)%*%(rmultinom(1,1, safe_multinom(om_1[,i]))))
    v_2=exp(lambda[1]+lambda[2]*P_obs_1)/((theta_1[3]+theta_1[4]*P_obs_1)^2)
    var_inv=1/sigma[p1]+1/v_2
    m_2=(Y_obs_1-theta_1[1]-theta_1[2]*P_obs_1)/(theta_1[3]+theta_1[4]*P_obs_1)
    P_mis[T1]=rnorm(T1,1/(var_inv)*(eta[p1]/sigma[p1]+m_2/v_2),1/sqrt(var_inv))
    
    # -----   P_IMP: post-treatent observed: P(t)   -----
    # level t=0 observed 
    #om_0=sapply(T0, function(i) omega(X=matrix_X[i,],beta=beta_0))
    #om_0[which(is.nan(om_0))]=0
    #p0__=sapply(1:n0, function(i) (1:n_cluster)%*%(rmultinom(1,1,om_0[,i])))
    #P_imp[T0]=rnorm(T0,eta[p0__],sigma[p0__])
    # level t=1 observed 
    #om_1=sapply(T1, function(i) omega(X=matrix_X[i,],beta=beta_1))
    #om_1[which(is.nan(om_1))]=0
    #p1__=sapply(1:n1, function(i) (1:n_cluster)%*%(rmultinom(1,1,om_1[,i])))
    #P_imp[T1]=rnorm(T1,eta[p1__],sigma[p1__])
    
    # ----------- Y model ----------- 
    
    # -----   THETA: means regression   -----
    # theta_0 = theta for treatment level 0
    P_tilde=cbind(1,P_obs_0)
    M=t(P_tilde)%*%diag(1/rep((exp(lambda[1]))^2,n0))%*%t(t(Y_obs_0))+p_theta[1]/(p_theta[2]^2)
    V=t(P_tilde)%*%diag(1/rep((exp(lambda[1]))^2,n0))%*%P_tilde+diag(rep(1/(p_theta[2]^2),2))
    V[lower.tri(V)] = t(V)[lower.tri(V)]
    theta_0=c(rmvnorm(1,solve(V)%*%M,solve(V)))
    
    # theta_1 = theta for treatment level 1
    P_tilde=cbind(1,P_obs_1,P_mis[T1],P_mis[T1]*P_obs_1)
    M=t(P_tilde)%*%diag(1/((exp(lambda[1]+lambda[2]*P_obs_1))^2))%*%t(t(Y_obs_1))+p_theta[1]/(p_theta[2]^2)
    V=t(P_tilde)%*%diag(1/((exp(lambda[1]+lambda[2]*P_obs_1))^2))%*%P_tilde+diag(rep(1/(p_theta[2]^2),4))
    V[lower.tri(V)] = t(V)[lower.tri(V)]
    theta_1=c(rmvnorm(1,solve(V)%*%M,solve(V)))
    
    # -----   LAMBDA: parameters in varainces   -----
    # lambda_0 = lambda involved in variance for treatment level 0 and level 1
    # lambda_1 = lambda involved in variance only for level 1
    
    l0_star=rnorm(1,p_lambda[1],p_lambda[2])  
    l1_star=rnorm(1,p_lambda[3],p_lambda[4])  
    
    mean_M=c(cbind(1,P_obs_0)%*%theta_0,P_tilde%*%theta_1)
    var_0=c(rep(exp(lambda[1]),n0),exp(lambda[1]+lambda[2]*P_obs_1))
    var_star_0=c(rep(exp(l0_star),n0),exp(l0_star+lambda[2]*P_obs_1))
    
    a0=min(1, exp(sum(dnorm(c(Y_obs_0,Y_obs_1),mean_M,var_star_0,log=T))-
                    sum(dnorm(c(Y_obs_0,Y_obs_1),mean_M,var_0,log=T))))
    if (runif(1)<a0){
      lambda[1]=l0_star
      acc_0=acc_0+1
    } 
    
    var_star_1=exp(lambda[1]+l1_star*P_obs_1)
    
    a1=min(1, exp(sum(dnorm(Y_obs_1,mean_M[(n0+1):n],var_star_1,log=T))-
                    sum(dnorm(Y_obs_1,mean_M[(n0+1):n],var_0[(n0+1):n],log=T))))
    if (runif(1)<a1){
      lambda[2]=l1_star
      acc_1=acc_1+1
    } 
    
    # -----   saving information   -----
    
    if(r>R_burnin){
      # --- imputing outcome ----
      P_0_r=rep(NA,n)
      P_0_r[T0]=P_obs_0
      P_0_r[T1]=P_mis[T1]
      Y_0_imp=rnorm(n,cbind(1,P_0_r)%*%t(t(theta_0)),
                    exp(lambda[1]))
      P_1_r=rep(NA,n)
      P_1_r[T1]=P_obs_1
      P_1_r[T0]=P_mis[T0]
      Y_1_imp=rnorm(n,cbind(1,P_1_r,P_0_r,P_0_r*P_1_r)%*%t(t(theta_1)),
                    exp(lambda[1]+lambda[2]*P_1_r))
      
      # --- save informations ----
      # parameters
      post_eta[,r-R_burnin]=eta
      post_var[,r-R_burnin]=sigma
      post_beta[,r-R_burnin]=c(beta_0,beta_1)
      post_theta[,r-R_burnin]=c(theta_0,theta_1)
      post_lambda[,r-R_burnin]=lambda
      # cluster allocation
      cluster_allocation_0[T0,r-R_burnin]=xi_0
      cluster_allocation_0[T1,r-R_burnin]=p1
      cluster_allocation_1[T1,r-R_burnin]=xi_1
      cluster_allocation_1[T0,r-R_burnin]=p0
      # imputation data
      post_P_0[,r-R_burnin]=P_0_r
      post_P_1[,r-R_burnin]=P_1_r
      post_Y_0_imp[,r-R_burnin]=Y_0_imp
      post_Y_1_imp[,r-R_burnin]=Y_1_imp
    }
    
    #print(r)
    # if (r%%500==0) print(paste0(r,"/",R," iterations"))
  }
  
  # -----   point estimation partition   -----
  # partition: cluster allocation
  S_all_cluster=partition_all_cluster(xi_0=cluster_allocation_0,xi_1=cluster_allocation_1)
  # partition: strata allocation
  S_strata_cluster=partition_strata_cluster(xi_0=cluster_allocation_0,xi_1=cluster_allocation_1, eta=post_eta)
  
  print(paste0("sample ",c, " done"))
  
  return(list(#post_eta=post_eta, post_var=post_var,
    #chains_theta=post_theta,
    S_all_cluster=S_all_cluster, S_strata_cluster=S_strata_cluster,
    post_theta=apply(post_theta,1,mean), 
    post_lambda=apply(post_lambda,1,mean),
    #acc_rate_0=acc_0/R, acc_rate_1=acc_1/R,
    #cluster_allocation_0=cluster_allocation_0, cluster_allocation_1=cluster_allocation_1,
    post_P_0_imp=apply(post_P_0,1,median, na.rm=TRUE), 
    post_P_1_imp=apply(post_P_1,1,median, na.rm=TRUE),
    post_Y_0_imp=apply(post_Y_0_imp,1,median, na.rm=TRUE), 
    post_Y_1_imp=apply(post_Y_1_imp,1,median, na.rm=TRUE)
  ))
}
