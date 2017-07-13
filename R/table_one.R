#' Produce formatted descriptives tables
#'
#' Theses functions mirror createTableOne, but allow for more customization.
#'
#' @param x,y Variables for univariate p-value calculation. Within \code{table_one}, these work out to be \code{x=data[[strata]], y=data[[vars[i]]]}.
#' @param vars Character vector. The column names of variables to summarize.
#' @param varlables Character vector. The labels for variables in the returned table. Should be the same length as \code{var}.
#' @param data Data.frame in which to look for variables.
#' @param strata Character. Stratifying variable.
#' @param normal Character vector. Variables to treat as normally distributed. If not included in this, numeric variables are considered non-normal.
#' @param exact Character vector. Variable to perform exact tests on.
#' @param fun_n_prc,fun_norm,fun_nonnorm Summary measure functions.
#' @param fun_apprx_p,fun_exact_p,fun_norm_p,fun_nonnorm_p P-value functions.
#' @param fun_p_fm Formatting function for p-values.
#' @param measurelab_nonnormal,measurelab_normal,measurelab_cat Character. Text to put in the row indicating how the variable is summarized.
#' @param sep Character. Text to separate the variable label from the measure label.
#' @param nspaces Integer. Number of spaces to indent factor levels.

#' @include summary_measures.R
#' @include formatting_functions.R

#' @export
table_one <- function(vars, varlabels=vars, data, strata, normal=NULL, exact=NULL, all_levels=FALSE,
                      fun_n_prc=n_perc,  fun_apprx_p=p_cat_apprx, fun_exact_p=p_cat_exact,
                      fun_norm=mean_sd, fun_nonnorm=median_iqr,  fun_norm_p=p_cont_norm,
                      fun_nonnorm_p = p_cont_nonnorm, fun_p_fmt = p_fmt,
                      measurelab_nonnormal="Median [IQR]", measurelab_normal="Mean±SD",
                      measurelab_cat="n (%)", sep=" -- ", nspaces=6) {

  is_cat    = sapply(data[vars], function(i) class(i) %in% c("logical","character","factor"))

  if(any(is_cat) & any(!is_cat)) {
    tbl <- rbind(
      cont_table(vars=vars[!is_cat], varlabels=varlabels[!is_cat], data=data, strata=strata, normal=normal,
                 fun_norm=fun_norm, fun_nonnorm=fun_nonnorm,  fun_norm_p=fun_norm_p,
                 fun_nonnorm_p = fun_nonnorm_p, fun_p_fmt = fun_p_fmt,
                 measurelab_nonnormal=measurelab_nonnormal,
                 measurelab_normal=measurelab_normal, sep=sep),
      cat_table(vars=vars[is_cat], varlabels=varlabels[is_cat], data=data, strata=strata, exact=exact,
                all_levels=all_levels, fun_n_prc=fun_n_prc,  fun_apprx_p=fun_apprx_p, fun_exact_p=fun_exact_p,
                fun_p_fmt = fun_p_fmt, measurelab_cat=measurelab_cat,sep=sep, nspaces=nspaces)
    )
  } else if(any(is_cat)) {
    tbl <-  cat_table(vars[is_cat], varlabels=varlabels[is_cat], data=data, strata=strata, exact=exact,
                      all_levels=all_levels, fun_n_prc=fun_n_prc,  fun_apprx_p=fun_apprx_p, fun_exact_p=fun_exact_p,
                      fun_p_fmt = fun_p_fmt, measurelab_cat=measurelab_cat,sep=sep, nspaces=nspaces)
  } else {
    tbl <- cont_table(vars[!is_cat], varlabels=varlabels[!is_cat], data=data, strata=strata, normal=normal,
                      fun_norm=fun_norm, fun_nonnorm=fun_nonnorm,  fun_norm_p=fun_norm_p,
                      fun_nonnorm_p = fun_nonnorm_p, fun_p_fmt = fun_p_fmt,
                      measurelab_nonnormal=measurelab_nonnormal,
                      measurelab_normal=measurelab_normal, sep=sep)
  }
  tbl
}


#' @export
#' @rdname table_one
cont_table <- function(vars, varlabels=vars, data, strata, normal=NULL,
                       fun_norm=mean_sd, fun_nonnorm=median_iqr,  fun_norm_p=p_cont_norm,
                       fun_nonnorm_p = p_cont_nonnorm, fun_p_fmt = p_fmt,
                       measurelab_nonnormal="Median [IQR]",
                       measurelab_normal="Mean±SD", sep=" -- ",...) {

  nvars <- length(vars)
  funs <- lapply(vars, function(i) if(i %in% normal) fun_norm else fun_nonnorm)  #get a list of measure functions to go with each variable
  p_funs <- lapply(vars, function(i) if(i %in% normal) fun_norm_p else fun_nonnorm_p) #get a list of p functions
  var_measures <- ifelse(vars %in% normal , measurelab_normal, measurelab_nonnormal) #vector of measure label functions to paste onto the rownames

  if(missing(strata)) {
    #if there is no strata variable, this is just one column and no p-value
    tbl <- t(t(sapply(1:nvars, function(i) funs[[i]](data[[vars[i]]]))  ))

  } else {
    #otherwise, it is a matrix with ncol=nlevels(strata) and an appropriate p value for var[i]
    tbl <- t(sapply(1:nvars, function(i) funs[[i]](data[[ vars[i] ]], data[[ strata ]] )  ))
    tbl <- cbind(tbl, p=sapply(1:nvars, function(i) fun_p_fmt(p_funs[[i]](data[[strata]],data[[vars[i]]]))))
  }
  rownames(tbl) <- paste0(varlabels, sep, var_measures) #rownames will be the same either way
  tbl
}

#' @export
#' @rdname table_one
cat_table <- function(vars, varlabels=vars, data, strata, all_levels=FALSE, exact=NULL,
                      fun_n_prc=n_perc,  fun_apprx_p=p_cat_apprx, fun_exact_p=p_cat_exact,
                      fun_p_fmt = p_fmt, measurelab_cat="n (%)",sep=" -- ", nspaces=6,...) {
  ncols    = if(missing(strata)) 1 else nlevels(factor(data[[strata]]))  #ncols doesn't include the p column for now
  nvars    = length(vars)
  nlevs    = sapply(vars, function(i) nlevels(data[[i]]))
  blank_row= rep("", ncols)
  spaces = paste(rep("&nbsp;", nspaces), collapse = "")

  if(missing(strata)) {
    #first deal with the simplest case -- missing strata.
    tbl      = t(t(unlist(lapply(1:nvars, function(i)
      if(nlevs[i]==2 & !all_levels) fun_n_prc(data[[vars[i]]])[2] else c(blank_row, fun_n_prc(data[[vars[i]]]))))))
  } else {

    p_funs   = lapply(1:nvars, function(i) if(vars[i] %in% exact) p_cat_exact else p_cat_apprx)
    p_vals    = sapply(1:nvars, function(i) fun_p_fmt(p_funs[[i]](data[[vars[i]]], data[[strata]])))
    p_row    = t(sapply(1:nvars, function(i) c(blank_row, p_vals[i])))
    list_tbls = vector("list", nvars)
    for(i in 1:nvars) {
      if(nlevs[i]==2 & !all_levels) {
        list_tbls[[i]] <- c(fun_n_prc(data[[vars[i]]], data[[strata]])[2,], p_vals[i])
      } else {
        list_tbls[[i]] <- rbind(p_row[i,], cbind(fun_n_prc(data[[vars[i]]], data[[strata]]),""))
      }
    }
      tbl      = do.call(rbind, list_tbls)
      colnames(tbl)[ncol(tbl)] <- "p-value"

  }

  var_measure_labs <- sapply(1:nvars, function(i)
    if(nlevs[i]==2 & !all_levels) paste0(varlabels[i], "=", levels(factor(data[[vars[i]]]))[2], sep, measurelab_cat)
    else paste0(varlabels[i], sep, measurelab_cat))

  rownames(tbl) = unlist(lapply(1:nvars, function(i)
    if(nlevs[i]==2 & !all_levels)  paste(var_measure_labs[i]) else c(var_measure_labs[i],paste(
    spaces, levels(factor(data[[vars[i]]])) ))))
  tbl
}


#' @export
#' @rdname table_one
p_cont_norm <- function(x,y) anova(lm(y~x))$`Pr(>F)`[1]
#' @export
#' @rdname table_one
p_cont_nonnorm <- function(x,y) kruskal.test(x,y)$p.value
#' @export
#' @rdname table_one
p_cat_exact <- function(x,y) fisher.test(x,y)$p.value
#' @export
#' @rdname table_one
p_cat_apprx <- function(x,y) chisq.test(x,y)$p.value
