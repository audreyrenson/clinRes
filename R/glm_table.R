#' Produce a formatted generalized linear model table
#'
#' This function returns a formatted table of generalized linear model results.
#'
#' @param fit An glm fit object of class \code{glm}.
#' @param digits Integer. The number of significant digits to return in coefficients
#' @param p.digits Integer. The numeric of significant digits to return in p-values
#' @param se Character. The type of standard error to use, "normal" or "robust".
#' @param intercept Logical. Whether or not to include the intercept row in the returned table.
#' @param fmt Logical. If true, passes the returned table to \code{fmt.glm_table}, returning a publication-ready table.
#' @param fun_coef Function to apply to the coefficients. By default, \code{exp} if link is "log" or "logit", otherwise identity.
#' @param ... Arguments passed to \code{fmt.glm_table}.
#' @return A matrix with coefficients, 95\% confidence intervals, and p-values.
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


#' @export
glm_table <- function(fit, digits=4, p.digits=digits+1, se="normal", intercept=FALSE, fmt=FALSE,
                      fun_coef = if(class(fit)[1]=="lm" || fit$family$link == c("identity")) I else exp,
                      id, ...) {
  # this will give you a neatly formatted table with exponentiated betas
  # from a glm object. Note: does not work with interaction terms
  # ... args passed to fmt.glm_table

  coefs <- if(!missing(id)) function(x) glm_robust_coefs.cluster(x, id=id) else if(se=="normal") glm_coefs else if(se=="robust") glm_robust_coefs

  tbl_coefs <- coefs(fit)

  tbl_coefs[,1:3] <- round(fun_coef(tbl_coefs[,1:3]), digits)
  tbl_coefs[,4] <- round(tbl_coefs[,4], p.digits)

  measure <- if(identical(fun_coef, I)) {"Beta"
  } else if(fit$family$link=="logit") {"OR"
  } else if(fit$family$family=="poisson" & !is.null(fit$offset)) {"IRR"
  } else if(fit$family$link=="log") {"RR"
  }

  colnames(tbl_coefs) <- c(measure,"upr","lwr","p-value")

  if(!intercept) tbl_coefs <- tbl_coefs[-1,,drop=FALSE]

  if(fmt) fmt.glm_table(tbl_coefs, digits=digits, p.digits=p.digits, ...) else tbl_coefs
}

#' @export
#' @rdname glm_table
fmt.glm_table <- function(glm_tbl, tbl_colnames = c(colnames(glm_tbl)[1], "95% CI", "p-value"),
                          tbl_rownames = rownames(glm_tbl), digits=2, p.digits=digits+1,
                          p_fmt = function(p) format.pval(round(as.numeric(p),p.digits), eps = .001),
                          beta_fmt = function(b) format(as.numeric(b), digits=digits, nsmall=digits),
                          ci_fmt = function(col1, col2) paste0(beta_fmt(col1), " to ", beta_fmt(col2)) ) {

  beta <- beta_fmt(glm_tbl[,1])
  ci   <- ci_fmt(glm_tbl[,2], glm_tbl[,3])
  p    <- p_fmt(glm_tbl[,4])

  tbl <- cbind(beta, ci, p)
  colnames(tbl) <- tbl_colnames
  rownames(tbl) <- tbl_rownames
  tbl

}

glm_table_indents <- function(fit, ref_txt="Ref", digits=4,
                              ref_num=if(fit$family$link %in% c("log","logit")) 1 else 0,  ...) {

  tbl <- glm_table(fit, digits = digits, ...)

  ref_num <- format(as.numeric(ref_num), digits=digits, nsmall=digits)

  INDENTS <- "&nbps"
  BLANKROW <- rep("", ncol(tbl))
  REFROW <- c(ref_num, rep(ref_txt, ncol(tbl)-1))

  vars <- as.character(attr(fit$terms, "variables"))[c(-1,-2)]
  dat <- fit$data[,vars, drop=FALSE]
  var_classes <- sapply(vars, function(i) class(dat[,i]))

  numeric_vars <- vars[var_classes %in% c("numeric","integer")]
  factor_vars <- vars[var_classes %in% c("factor","character","logical")]

  var_rownums <- sapply(vars, function(i) if(i %in% numeric_vars) 1 else length(fit$xlevels[[i]]) + 1 )
  var_rownames <- sapply(vars, function(i) if(i %in% numeric_vars) i else c(i, fit$xlevels[[i]]))

  list_tblrows <- vector("list", length = length(vars))
  names(list_tblrows) <- vars

  list_tblrows <- lapply(vars, function(i)
    if(i %in% rownames(tbl)) tb <- tbl[i,,drop=FALSE] else {
      tb <- rbind(BLANKROW, REFROW, tbl[paste0(i, var_rownames[[i]][c(-1,-2)]) ,])
      rownames(tb) <- paste0(c("",rep(INDENTS, nrow(tb)-1)), var_rownames[[i]])
      tb
    }
  )


  list_tblrows

  tbl_out <- do.call(rbind, list_tblrows)

  tbl_out

}



