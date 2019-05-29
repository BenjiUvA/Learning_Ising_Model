beta_vec <- rep(c(seq(0,2,1/199.5),seq(2,0,-1/199.5),rep(0,400)),5)

LIMA_sim2 <- function(n = 10, tau = rep(0,n), omega = matrix(0,n,n), X = sample(c(-1,1),n,T), nreps = 6000, beta = 1, Hebb = T) {

  dat <- matrix(0, nrow = nreps, ncol = n)
  omega_save <- list(nreps)
  gibbs_entropy <- numeric(nreps)
  did_flip <- numeric(nreps)

  bm <- as.matrix(expand.grid(rep(list(0:1),n)))
  bm[bm==0] <- -1
  X1 <- X

  prog = dplyr::progress_estimated(nreps)
  for (i in 1:nreps) {

    beta_use <- beta[i]
    omega_save[[i]] <- omega
    dat[i,] <- X1

    g <- sample(1:10,1)

    p <- p_flip(tau = tau,omega = omega,X = X1,g = g,beta = beta_use)

    if(p > runif(1)) {
      X1[g] <- -X1[g]
      did_flip[i] <- 1
    }

    pi_bm <- probability_bm(omega = omega,tau = tau,n = n,beta = beta_use,bm = bm)

    gibbs_entropy[i] <- entropyS(omega = omega,tau = tau,X = X1,beta = beta_use,pi_bm = pi_bm)

    omega <- delta_om(omega = omega,X = X1)
    omega <- omega[[2]]


    prog$tick()$print()
  }
}


