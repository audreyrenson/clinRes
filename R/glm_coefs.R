glm_coefs <- function(fit) {
  s <- summary(fit)
  if(!"zeroinfl" %in% class(fit)) {

    tbl <- cbind(
      coef(s)[,1],
      coef(s)[,1] - 1.96*coef(s)[,2],
      coef(s)[,1] + 1.96*coef(s)[,2],
      coef(s)[,4]
    )
  } else if(fit$dist == "negbin") {

    pvals_count <- summary(fit)$coefficients$count[,4]
    pvals_zero <- summary(fit)$coefficients$zero[,4]

    tbl <- cbind(
      coef(fit),
      confint(fit),
      c(pvals_count[1:length(pvals_count)-1], pvals_zero)
    )
  }
  tbl
}

glm_robust_coefs <- function(fit) {
  cov.fit <- sandwich::vcovHC(fit, type="HC0")
  std.err <- sqrt(diag(cov.fit))
  coefs <- na.omit(coef(fit))
  tbl <- cbind(coefs,
               coefs - 1.96 * std.err,
               coefs + 1.96 * std.err,
               2 * pnorm(-abs(coefs/std.err)))

  tbl
}

glm_robust_coefs.cluster <- function(fit,id) {
  stopifnot(id %in% names(fit$data))

  #get vector of id variable for samples in model
  vect_id <- as.factor(fit$data[rownames(model.frame(fit)), id])

  cov.fit <- cl.vcov(fit, vect_id)
  std.err <- sqrt(diag(cov.fit))
  coefs <- na.omit(coef(fit))
  tbl <- cbind(coefs,
               coefs - 1.96 * std.err,
               coefs + 1.96 * std.err,
               2 * pnorm(-abs(coefs/std.err)))

  tbl
}


cl.vcov   <- function(fit, id){
 # attach(dat, warn.conflicts = F)
  require(sandwich)
  M <- length(unique(id))
  N <- length(id)
  K <- fit$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fit),2, function(x) tapply(x, id, sum));
  vcovCL <- dfc*sandwich(fit, meat=crossprod(uj)/N)
  vcovCL
}

