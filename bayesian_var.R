#----------Function to estimate BVAR using Gibbs sampler for FAVAR model---------------
# Author: Pritha Chaudhuri
# Date: 6/16/2020
#---------------------------------------------------------------------------------------

bayesian_var <- function(data,nsim,burnin,p,n_hz){
  start_time <- Sys.time()
  ## Data: TT rows and n columns. TT is the number of periods in the data. 
  # n is the number of variables in the VAR system. The last 2 variables are the federal
  # funds rate and the unconventional policy index resp.
  TT <- nrow(data)
  n <- ncol(data) 
  
  ## Preliminaries
  p <- p # number of lags of dependent variables
  nsim <- nsim # number of simulations for Gibbs sampler
  burnin <- burnin # burn-in periods to discard
  k <- n*p+1 # number of coefficients to estimate in each equation
  n_hz <- n_hz
  
  ## Priors
  nu0 <- n+3
  S0 <- diag(n)
  beta0 <- matrix(0, nrow = n*k, ncol = 1)
  
  ## Data Handling
  # Save first 2p obs as initial conditions
  Y0 <- as.matrix(data[1:(2*p), ])
  Y <- as.matrix(data[(2*p+1):TT, ])
  tt <- nrow(Y)
  y <- matrix(t(Y), nrow = (tt*n), ncol = 1) # converts Y into a single column by rows
  
  # Precision for coefficients = 1, for intercepts = 1/10
  # Starting at 1, every kth (here 36th) element is an intercept
  tmp <- matrix(1, nrow = n*k, ncol = 1)
  tmp[seq(1, length(tmp), k)] <- 1/10
  iVbeta <- as.matrix(sparseMatrix(i = 1:(n*k), j = 1:(n*k), x = as.numeric(tmp)))
  
  # Lagging data to create regressors
  tmpY <- rbind(Y0[(nrow(Y0)-p+1):nrow(Y0), ], Y) # entire data lagged p
  X_tilde <- matrix(0, nrow = tt, ncol = (n*p)) # ncols=np since each variable is lagged p times
  for (i in 1:p) {
    X_tilde[, ((i-1)*n+1):(i*n)] = tmpY[(p-i+1):(nrow(tmpY)-i), ]
  }
  X_tilde <- cbind(matrix(1, nrow = tt, ncol = 1), X_tilde)
  X <- SURform2(X_tilde, n)
  
  ## Gibbs Sampler
  # Intialize storage 
  store_Sig <- array(1, c(nsim, n, n))
  store_beta <- matrix(0, nrow = nsim, ncol = n*k)
  store_yIR <- matrix(0, nrow = n_hz, ncol = n)
  store_yIR_sim <- array(0, c(nsim, n_hz, n))
  
  # Initialize chain
  betta <- solve(t(X)%*%X) %*% (t(X)%*%y)
  e <- matrix((y-(X%*%betta)), nrow = n, ncol = tt)
  Sig <- (e %*% t(e)) / tt
  iSig <- solve(Sig)
  
  for (isim in 1:(nsim+burnin)) {
    # sample beta
    XiSig <- t(X) %*% kronecker(diag(tt), iSig)
    XiSigX <- XiSig %*% X
    Kbeta <- iVbeta + XiSigX
    beta_hat <- solve(Kbeta) %*% (iVbeta%*%beta0 + XiSig%*%y)
    betta <- beta_hat + solve(chol(Kbeta)) %*% rnorm(n*k)
    
    # sample Sig
    e <- matrix((y-(X%*%betta)), nrow = n, ncol = tt)
    Sig <- riwish(v = (nu0+tt), S = (S0+(e%*%t(e))))
    iSig <- solve(Sig)
    
    # Print counter every 1000 loops
    if(isim %% 500 == 0){
      cat(isim, "loops \n")
    }
    
    # Store parameters
    if(isim>burnin){
      isave <- isim-burnin
      store_beta[isave, ] <- as.matrix(t(betta))
      store_Sig[isave, , ] <- as.matrix(Sig)
      
      # compute impulse response
      CSig <- t(chol(Sig))
      shock <- matrix(c(0,0,0,0,1), nrow = n, ncol = 1) / CSig[n,n]
      yIR <- construct_IR(betta, Sig, n_hz, shock)
      store_yIR <- store_yIR + yIR
      store_yIR_sim[isave, ,] <- yIR
    }
    
  }
  
  end_time <- Sys.time()
  end_time - start_time
  
  betta_hat <- as.matrix(colMeans(store_beta))
  # betta_95 <- as.matrix(apply(store_beta, 2, function(x) quantile(x, probs = .95)))
  # betta_05 <- as.matrix(apply(store_beta, 2, function(x) quantile(x, probs = .05)))
  Sig_hat <- as.matrix(colMeans(store_Sig))
  # Sig_95 <- as.matrix(apply(store_Sig, c(2,3), function(x) quantile(x, probs = .95)))
  # Sig_05 <- as.matrix(apply(store_Sig, c(2,3), function(x) quantile(x, probs = .05)))
  yIR_hat <- store_yIR/nsim
  # yIR_upper <- apply(store_yIR_sim, c(2,3), function(x) quantile(x, probs = .95))
  # yIR_lower <- apply(store_yIR_sim, c(2,3), function(x) quantile(x, probs = .05))
  
  bvar_result <- list(betta_hat, Sig_hat, yIR_hat)
  
  return(bvar_result)
  
}

#----------Function to create matrix X----------------------------------------------
SURform2 <- function(X, n){
  repX <- kronecker(X, rep(1, n))
  r <- nrow(X)
  c <- ncol(X)
  idi <- kronecker((1:(r*n)), rep(1, c))
  idj <- kronecker(rep(1, r), (1:(n*c)))
  x <- matrix(t(repX), nrow = n*r*c, ncol = 1)
  X_out <- as.matrix(sparseMatrix(i = idi, j = idj, x = as.numeric(x)))
  
  return(X_out)
}

#----------Function to generate impulse response------------------------------------
construct_IR <- function(betta, Sig, n_hz, shock){
  nn <- nrow(Sig)
  pp <- (nrow(betta)/nn - 1)/nn
  CSig <- t(chol(Sig))
  
  tmpZ1 <- matrix(0, nrow = pp, ncol = nn)
  tmpZ <- matrix(0, nrow = pp, ncol = nn)
  Yt1 <- CSig %*% shock # shock hits in period 0
  Yt <- matrix(0, nrow = nn, ncol = 1)
  yIR <- matrix(0, nrow = n_hz, ncol = nn)
  yIR[1, ] <- t(Yt1)
  
  for (t in 2:n_hz) {
    # update regressors
    tmpZ1 <- rbind(t(Yt1), tmpZ1[1:(nrow(tmpZ1)-1), ]) # no shock hits
    tmpZ <- rbind(t(Yt), tmpZ[1:(nrow(tmpZ)-1), ]) # shock hits
    
    # evolution of variables if shock hits
    ee <- CSig %*% rnorm(nn)
    Z1 <- matrix(t(tmpZ1), nrow = 1, ncol = (nn*pp))
    Xt1 <- kronecker(diag(nn), cbind(c(1), Z1))
    Yt1 <- Xt1 %*% betta + ee
    
    # evolution of variables if shock does not hit
    Z <- matrix(t(tmpZ), nrow = 1, ncol = (nn*pp))
    Xt <-  kronecker(diag(nn), cbind(c(1), Z))
    Yt <- Xt %*% betta + ee
    
    # IR is the difference of the two scenarios
    yIR[t, ] <- as.matrix(t(Yt1 - Yt))
  }
  
  return(yIR)
}

