#' Boltzmann probabilities for all states
#'
#'Calculates Boltzmann probabilities for all states
#' @param tau is a vector with a threshold for each node
#' @param omega is a (symmetrical) matrix with the strength of connection between each nodes. Set the the diagonal to zero.
#' @param n is the number of nodes in the network
#' @param beta is the dependency parameter
#' @param bm is a matrix with all possible configurations. This can be created with: bm <- as.matrix(expand.grid(rep(list(0:1),n))); bm[bm==0] <- -1
#'
#' @export

probability_bm <- function(omega, tau, n, beta = 1, bm) {

  pi_bm <- rep(0, 2^n)

  for(i in 1:length(pi_bm)) {
    pi_bm[i] <- exp(-beta*ham(tau = tau, omega = omega, X = bm[i,]))
  }

  sum_pi_bm <- sum(pi_bm)

  pi_bm <- pi_bm/sum_pi_bm

  return(pi_bm)
}
