#' Simulation 1
#'
#' @export

sim1_function <- function(n = 10, nreps = 5000, tau, omega, beta, X1) {


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


    omega <- delta_om(omega = omega, X = X1) # delta_om returns delta omega and omega new
    omega <- omega[[2]] # omega[[2]] returns the updated omega

    prog$tick()$print()
  }

  omega_mean <- numeric(nreps)
  for(i in 1:nreps) {
    omega_mean[i] <- mean(omega_save[[i]])
  }

  output <- list("configurations" = dat, "omega's" = omega_save, "mean omega's" = omega_mean, "did flip?" = did_flip)
  return(output)
}
