test_that("ham", {

  set.seed(42)
  p <- 10
  tau <- runif(p, -1, 1)
  X <- matrix(runif(p))
  omega <- matrix(stats::rnorm(p^2), p, p)
  omega <- tcrossprod(omega)

  diag(omega) <- 0
  expect_equal(
    ham(tau = tau, X = X, omega = omega),
    expected = structure(6.06721543218154, .Dim = c(1L, 1L))
  )

})
