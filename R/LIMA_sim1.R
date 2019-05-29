#' Simulation 1
#'
#' This function resembles simulation 1 from the LIMA chapter. It generates data for nreps iterations, based on an initial configuration (X1). At each iteration, a random node is flipped with probability p_flip and omega is updated with Hebb's rule (if Hebb=T).
#'
#' @param n are the number of nodes
#' @param nreps are the number of iterations
#' @param tau a vector with thresholds
#' @param omega a (symmetric) matrix with edge weights (diag==0)
#' @param beta is the dependency parameter
#' @param X1 is the initial configuration of the network
#' @param Hebb apply Hebb's rule (logical)
#'
#' @export

LIMA_sim1 <- function(n = 10, nreps = 5000, tau, omega, beta, X1, Hebb = TRUE) {

  if(!is.numeric(n) | !is.numeric(nreps) | !is.numeric(tau) | !is.numeric(X1) | length(X1)!=n | !is.numeric(beta)) {
   stop("One of your arguments that should be nummeric is nonnummeric")
  }

  omega_save <- list(nreps)
  dat <- matrix(0,nrow = nreps, ncol = n)
  did_flip <- numeric(nreps)



  prog = dplyr::progress_estimated(nreps)
  for(i in 1:nreps) {

    g <- sample(1:n,1)
    p <- p_flip(tau = tau,omega = omega,X = X1, g = g)
    if (p < runif(1)) {
      X1[g] <- X1[g]*-1
      did_flip[i] <- 1
    }

    omega_save[[i]] <- omega
    dat[i,] <- X1

    if (Hebb) {
      omega <- delta_om(omega = omega, X = X1) # delta_om returns delta omega and omega new
      omega <- omega[[2]] # omega[[2]] returns the updated omega
    }

    prog$tick()$print()
  }

  omega_mean <- numeric(nreps)
  for(i in 1:nreps) {
    omega_mean[i] <- mean(omega_save[[i]])
  }

  output <- list("configurations" = dat, "omega's" = omega_save, "mean omega's" = omega_mean, "did flip?" = did_flip)
  return(output)
}
