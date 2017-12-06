#' @export
reg_autoindent <- function(fit, nspaces=6, p.adjust.method=NULL, apply.coef=NULL,
                           digits.coef=2, digits.p=3, intercept=FALSE) {
  #works for lm and glm
  #padj passed to p.adjust(method = ...)
  # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
  #   "fdr", "none")
  # apply.coef is a function to apply to the coefficients and confidence intervals
  # intercept is whether or not to keep the intercept in the table

  # extract coefficients and p-values, format, and correct if applicable
  if(!is.null(p.adjust.method)) {
    require(multcomp)
    coefs <- confint(glht(fit, linfct=diag(length(coef(fit)))))$confint
    p_val <- format(p.adjust(summary(fit)$coefficients[,4], method = p.adjust.method),
                    digits=digits.p)
  } else {
    coefs <- cbind(coef(fit), confint(fit))
    p_val <- format(summary(fit)$coefficients[,4], digits=digits.p)
  }

  # apply a function to the coefficients if applicable
  if(!is.null(apply.coef)) {
    coefs <- format(apply(coefs, 2, apply.coef), digits = digits.coef)
  } else {
    coefs <- format(coefs, digits=digits.coef)
  }

  # assemble the table
  tbl_out <- cbind(coefs, p_val)

  # remove the intercept if applicable
  if(!intercept) {
    tbl_out <- tbl_out[-1,]
  }

  # add indents
  varnames <- names(fit$model)[-1]
  rnames <- rownames(tbl_out) %>% as.character

  associated_varnames <-
    sapply(rnames, function(rname)
      varnames[
        which(sapply(varnames, function(varname)
          grep(varname, rname)
        ) == 1)
        ]
    ) %>% as.character
  n_levels <- as.numeric(table(associated_varnames)[associated_varnames])
  is_numeric <- rnames == associated_varnames

  var_info <- data.frame(rnames, associated_varnames, n_levels, is_numeric)
  new_rnames <- sapply(1:length(rnames), function(i)
    ifelse(is_numeric[i], rnames[i],
           gsub(pattern = associated_varnames[i], x = rnames[i], replacement = ""))
  )



  #function to insert a row in a dataframe
  insertRow <- function(existingDF, newrow, r) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    existingDF
  }

  #index of whether or not a row has been added for this variable
  row_added <- rep(FALSE, length(rnames))

  for(i in seq_along(rnames)) {
    if(is_numeric[i]) {
      #skip
    } else {
      if(!row_added[i]) {
        #if a row hasn't already been added to this variable
        #add a row above

        ##############
        # Left off 2.16.17.
        # Need to figure out the best way to add a row and keep the index working properly.
        #############

        #and assign TRUE every element of row_added with this associated variable name
        row_added[associated_varnames == associated_varnames[i]] <- TRUE
      }
    }
  }
  return(tbl_out)
}
