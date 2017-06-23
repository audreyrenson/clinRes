#' @export
glm_table <- function(fit, digits=4, p.digits=digits+1, se="normal", intercept=FALSE,
                      fun_coef = if(fit$family$link %in% c("log","logit")) exp else I) {
  # this will give you a neatly formatted table with exponentiated betas
  # from a glm object. Note: does not work with interaction terms

  if(!"zeroinfl" %in% class(fit)) {

    pvals <- round(coef(summary(fit))[,4], digits=p.digits)
    tbl <- cbind(
      round(fun_coef(coef(fit)), digits=digits),
      round(fun_coef(confint(fit)),digits=digits),
      pvals
    )

  } else if(fit$dist == "negbin") {

    pvals_count <- ifelse(summary(fit)$coefficients$count[,4] < 0.0001, "< 0.0001",
                          as.character(round(summary(fit)$coefficients$count[,4], digits=p.digits)))
    pvals_zero <- ifelse(summary(fit)$coefficients$zero[,4] < 0.0001, "< 0.0001",
                         as.character(round(summary(fit)$coefficients$zero[,4], digits=p.digits)))

    tbl <- cbind(
      round(fun_coef(coef(fit)), digits=3),
      round(fun_coef(confint(fit)),digits=3),
      c(pvals_count[1:length(pvals_count)-1], pvals_zero)
    )
  }

  measure <- if(fit$family$link=="logit") {"OR"
      } else if(fit$family$link=="log" & fit$family$family=="binomial") {"RR"
      } else if(fit$family$link=="log" & fit$family$family=="poisson") {"IRR"
      } else if(fit$family$link=="identity") {"Beta"}

  colnames(tbl) <- c(measure,"upr","lwr","p-value")

  if(!intercept) tbl <- tbl[-1,,drop=FALSE]
  tbl
}

