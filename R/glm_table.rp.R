#' @export
glm_table.rp <- function(poisson_fit, RR=TRUE) {
  # Poisson regression with robust variance estimate to model risk ratios.
  # poisson_fit is a glm object with format:
  # glm(y~x, data=data, family=poisson(link="log"))
  # RR=FALSE will yield unexponentiated results, otherwise
  # risk ratios and associated confidence intervals are returned.


  if(RR) {
    ex <- function(x) exp(x)

  } else {
    ex <- function(x) x
  }

  cov.poisson_fit <- sandwich::vcovHC(poisson_fit, type="HC0")
  std.err <- sqrt(diag(cov.poisson_fit))
  r.est <- cbind(RR= ex(coef(poisson_fit)),
                 "95%" = ex(coef(poisson_fit) - 1.96 * std.err),
                 "CI  " = ex(coef(poisson_fit) + 1.96 * std.err),
                 "p-value" = 2 * pnorm(abs(coef(poisson_fit)/std.err), lower.tail=FALSE))

  round(r.est, 6)
}
