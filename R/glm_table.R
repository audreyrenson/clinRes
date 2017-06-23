#' @export
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

glm_coefs <- function(fit) {
  if(!"zeroinfl" %in% class(fit)) {

    tbl <- cbind(
      coef(fit),
      confint(fit),
      coef(summary(fit))[,4]
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
  tbl <- cbind(coef(fit),
               coef(fit) - 1.96 * std.err,
               coef(fit) + 1.96 * std.err,
               2 * pnorm(abs(coef(fit)/std.err), lower.tail=FALSE))

  tbl
}


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



