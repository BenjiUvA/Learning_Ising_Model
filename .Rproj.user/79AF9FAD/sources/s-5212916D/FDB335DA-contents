#' Boltzmann probabilities for all states
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
