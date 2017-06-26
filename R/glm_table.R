#' @export
#'
#' Produce publication-ready tables from glm objects.
#'
#' @param fit An glm fit object of class \code{glm}.
#' @param digits Integer. The number of significant digits to return in coefficients
#' @param p.digits Integer. The numeric of significant digits to return in p-values
#' @param se Character. The type of standard error to use.
#' #' \describe{
#'   \item{normal}{Asymptotic normal standard errors, as used by \code{confint}.}
#'   \item{robust}{Robust sandwich standard errors, as implemented in package \code{sandwich}}
#' }
#' @param intercept Logical. Whether or not to include the intercept row in the returned table.
#' @param fmt Logical. If true, passes the returned table to \code{fmt.glm_table}, returning a publication-ready table.
#' @param fun_coef Function to apply to the coefficients. By default, \code{exp} if link is "log" or "logit", otherwise identity.
#' @param ... Arguments passed to \code{fmt.glm_table}.
#' @return A matrix with coefficients, 95% confidence intervals, and p-values.
#' @examples
#' ## Basic logistic regression example
#' n <- 50
#' x <- rnorm(n)
#' y <- sample(c(TRUE, FALSE), size=n, replace=TRUE)
#' fit <- glm(y~x, family=binomial)
#' glm_table(fit)
#'
#' ## Use se="robust" to get risk ratios from poisson regression
#' fit <- glm(y~x, family=poisson)
#' glm_table(fit, se="robust")
#'
#' ## Get a nicely formatted table
#' glm_table(fit, fmt=TRUE)
#'
#'
glm_table <- function(fit, digits=4, p.digits=digits+1, se="normal", intercept=FALSE, fmt=FALSE,
                      fun_coef = if(fit$family$link %in% c("log","logit")) exp else I,
                      ...) {
  # this will give you a neatly formatted table with exponentiated betas
  # from a glm object. Note: does not work with interaction terms
  # ... args passed to fmt.glm_table

  coefs <- if(se=="normal") glm_coefs else if(se=="robust") glm_robust_coefs

  tbl_coefs <- coefs(fit)

  tbl_coefs[,1:3] <- round(fun_coef(tbl_coefs[,1:3]), digits)
  tbl_coefs[,4] <- round(tbl_coefs[,4], p.digits)

  measure <- if(fit$family$link=="logit") {"OR"
  } else if(fit$family$link=="log" & fit$family$family=="binomial") {"RR"
  } else if(fit$family$link=="log" & fit$family$family=="poisson") {"IRR"
  } else if(fit$family$link=="identity") {"Beta"}

  colnames(tbl_coefs) <- c(measure,"upr","lwr","p-value")

  if(!intercept) tbl_coefs <- tbl_coefs[-1,,drop=FALSE]

  if(fmt) fmt.glm_table(tbl_coefs, digits=digits, p.digits=p.digits, ...) else tbl_coefs
}




