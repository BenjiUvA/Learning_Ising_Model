#'LIM_simulator
#'
#'Simulates data from the Learning Ising Model
#'
#' @param n is the number of nodes in the network
#' @param nreps is the number of iterations
#' @param tau is a vector with a threshold for each node
#' @param omega is a (symmetrical) matrix with the strength of connection between each nodes. Set the the diagonal to zero.
#' @param X1 is the initial configuration of the network (e.g. if there are three nodes, X1 can be c(-1,1,-1))
#' @param beta is the dependency parameter in the Ising model
#' @param Hebb do you want to update omega with Hebb's rule?
#' @param e is the learning parameter of Hebb's rule
#' @param lambda is the decay parameter of Hebb's rule
#'
#' @return configurations contains all the configurations of the network over all nreps iterations
#' @return beta contains a vector of length()==nreps with values used for beta
#' @return mean omega contains a vector with the mean of omega at each configuration
#' @return omega t=... contains omega at 5 timepoints between the first and the last iteration (equal intervals)
#' @return gibbs entropy a vector with gibbs entropy for each iteration

#'@export

LIM_simulator <- function(n = 10, nreps = 100, tau, omega, beta, X1 = sample(c(-1,1),n,T), Hebb = T, e =.001, lambda = .001) {



  if(n <= 0) {
    stop("make sure n is a positive number")
  }

  if(nreps <= 0) {
    stop("make sure nreps is a positive integer")
  }

  if(length(beta)!=1 && length(beta)!=nreps || !is.numeric(beta)) {
    stop("make sure length(beta) is equal to 1 or nreps")
  }

  if(length(tau)!=n || !is.numeric(tau)) {
    stop("make sure that length(tau)==n")
  }

  if(nrow(omega)!=n || ncol(omega)!=n || all(diag(omega)!=0) || !is.numeric(omega) || omega[lower.tri(omega)]!=t(omega)[lower.tri(omega)]) {
    stop("make sure that omega is a symmetrical matrix with dimensions n*n and diag==0")
  }

  if(length(X1)!=n) {
    stop("length(X1) must be n")
  }


  beta_use <- beta

  if(length(beta)==1) {
    beta_use <- rep(beta,nreps)
  }

  # wat ik nu heb: n, nreps, tau, omega, beta, X1


  bm <- as.matrix(expand.grid(rep(list(0:1),n)))
  bm[bm==0] <- -1

  omega_save <- list(nreps)
  dat <- matrix(0,nrow = nreps, ncol = n)
  gibbs_entropy <- numeric(nreps)
  did_flip <- numeric(nreps)
  omega_mean <- numeric(nreps)


  prog = dplyr::progress_estimated(nreps)
  for(i in 1:nreps) {

    omega_save[[i]] <- omega

    dat[i,] <- X1

    pi_bm <- probability_bm(omega = omega,tau = tau,n = n,beta = beta_use[i],bm = bm)

    gibbs_entropy[i] <- entropyS(omega = omega,tau = tau,X = X1,beta = beta_use[i],pi_bm = pi_bm)

    g <- sample(1:10,1,T)

    p <- p_flip(tau = tau, omega = omega, X = X1, g = g, beta = beta_use[i])

    if(p > runif(1)){
      X1[g] <- -X1[g]
      did_flip[i] <- 1
    }

    if(Hebb) {
      omega <- delta_om(omega = omega,X = X1,e = e,lambda = lambda)
      omega <- omega[[2]]
    }
  prog$tick()$print()
  }

  for(i in 1:nreps) {
    omega_mean[i] <- mean(omega_save[[i]])
  }

  omega_out <- list(5)
  if(nreps>5) {
    omega_out[[1]] <- omega_save[[1]]
    omega_out[[2]] <- omega_save[[0.25*nreps]]
    omega_out[[3]] <- omega_save[[0.50*nreps]]
    omega_out[[4]] <- omega_save[[0.75*nreps]]
    omega_out[[5]] <- omega_save[[nreps-1]]
  }

  output <- list("configurations" = dat, "beta" = beta_use, "mean omega" = omega_mean, "omega t=1" = omega_out[[1]],"omega t=0.25*nreps" = omega_out[[2]],"omega t=0.50*nreps" = omega_out[[3]], "omega t=0.75*nreps" = omega_out[[4]], "omega t=nreps-1" = omega_out[[5]], "gibbs entropy" = gibbs_entropy)

  return(output)

}
