#' Delta Tau
#'
#'
#'@export


delta_t <- function(tau,X,e = .001,l = .001) {

  delta_t <- numeric(length(tau))

  for(i in 1:length(tau)) {
    delta_t[i] <- e*(1-abs(tau[i]))*X[i]-l*tau[i]
  }

  tau_new <- tau+delta_t
  return(tau_new)
}

