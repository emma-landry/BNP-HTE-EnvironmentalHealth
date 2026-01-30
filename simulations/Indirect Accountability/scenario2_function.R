# Taken from https://github.com/ebprado/BCF-discussion-paper
library(dbarts)

g <- function(x){
  ifelse(x == 1, 2, ifelse(x == 2, -1, -4))
}

generate_data <- function(n, p, tau, mu, seed){
  
  set.seed(seed)
  
  x <- matrix(rnorm(n * p), nrow = n, ncol = p)
  x[, 1] <- rnorm(n)
  x[, 2] <- rnorm(n)
  x[, 3] <- rnorm(n)
  x[, 4] <- rbinom(n, 1, 0.5)
  x[, 5] <- sample(c(1, 2, 3), n, replace = TRUE)
  u <- runif(n, 0, 1)
  sigma <- 1
  
  # Tau -------- 
  if (tau == 'homogeneous'){tau_x = 3}
  if (tau == 'heterogeneous'){tau_x = 1 + 2 * x[, 2]*x[, 4]} # x[,4] after talking to Eoghan. It makes sense!
  
  # Mu -----------
  if (mu == 'linear'){mu_x = 1 + g(x[,5]) + x[, 1]*x[, 3]}
  if (mu == 'nonlinear'){mu_x = -6 + g(x[,5]) + 6 * abs(x[, 3] - 1)}
  
  # Pi and Pihat -------- 
  pi.x <- 0.8 * pnorm((3 * mu_x / sd(mu_x)) - 0.5 * x[,1]) + 0.05 + u / 10
  z <- rbinom(n, 1, pi.x)
  # pihat <- apply(bart(x, z, verbose=FALSE)$yhat.train, 2, mean)
  
  # Response variable
  eps0 <- rnorm(n, sd = sigma)
  eps1 <- rnorm(n, sd = sigma)
  
  y0 <- mu_x + eps0              # Y(0)
  y1 <- mu_x + tau_x + eps1      # Y(1)
  
  y_mat <- rbind(y0, y1)
  
  return(list(data = list(Y = y_mat,
                          X = x,
                          T = z),
              tau = tau_x,
              mu = mu_x,
              # pihat = pihat,
              n = n,
              p = p))
}
