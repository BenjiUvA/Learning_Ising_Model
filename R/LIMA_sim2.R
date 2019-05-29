#' Simuation 2
#'
#'
#'
#'
#
# beta_vec <- rep(c(seq(0,2,1/199.5),seq(2,0,-1/199.5),rep(0,400)),5)
#
# nreps <- length(beta_vec)
# n <- 10
#
# # initiele omega:
# omega <- rnorm(n*(n-1)/2,mean = .05, sd = .01)
# omega <- matrix(omega,n,n)
# omega[lower.tri(omega)] <- t(omega)[lower.tri(omega)]
# diag(omega) <- 0
#
#
# # include this code to resemble an omega that updates itself from .05 to .17:
#
# omega_fixed <- list(nreps) # to resemble the Hebbian updated omega in the chapter
# omega_fixed_mean <- seq(.05,.17,(.17-.05)/(nreps-1))
# #
# for(i in 1:nreps) {
#   omega_fixed[[i]] <- rnorm(n*(n-1)/2,mean = omega_fixed_mean[i], sd = .01)
#   omega_fixed[[i]] <- matrix(omega_fixed[[i]],n,n)
#   omega_fixed[[i]][lower.tri(omega_fixed[[i]])] <- t(omega_fixed[[i]])[lower.tri(omega_fixed[[i]])]
#   diag(omega_fixed[[i]]) <- 0
# }
#
# Hebb <- T # want to simulate with(out) Hebbian learning?
#
# tau <- rep(0,n)
# dat <- matrix(0, nrow = nreps, ncol = n)
# omega_save <- list(nreps)
# gibbs_entropy <- numeric(nreps)
#
# #X1 <- rep(-1,n)
#
# X1 <- sample(c(-1,1),n,T)
#
# bm <- as.matrix(expand.grid(rep(list(0:1),n)))
# bm[bm==0] <- -1
#
# i <- 2
#
# prog = dplyr::progress_estimated(nreps)
# for(i in 1:nreps) {
#
#   beta <- beta_vec[i]
#
#   #omega <- omega_fixed[[i]] # de gesimuleerde omega's (mean loopt van .05 tot .17)
#
#   g <- sample(1:n,1)
#
#   p <- p_flip(tau = tau, omega = omega, X = X1, g = g, beta = beta)
#
#   if (p > runif(1)) {
#     X1[g] <- X1[g]*-1
#   }
#
#   pi_bm <- probability_bm(omega = omega, tau = tau, n = n, beta = beta, bm = bm) # boltzmann probabilities
#
#   gibbs_entropy[i] <- entropyS(omega = omega, tau = tau, beta = beta, pi_bm = pi_bm, n = n)
#
#   if(Hebb) {
#
#     omega <- delta_om(omega = omega, X = X1) # delta_om returns delta omega and omega new
#     omega <- omega[[2]] # omega[[2]] returns the updated omega
#
#     omega_save[[i]] <- omega
#
#   }
#   dat[i,] <- X1
#   prog$tick()$print()
# }
#
#
# omega_mean <- numeric(length(omega_save))
# for(i in 1:length(omega_save)){ # save the mean of all omega's after Hebb
#   omega_mean[i] <- mean(omega_save[[i]])
# }
