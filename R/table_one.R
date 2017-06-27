#' Produce formatted descriptives tables
#'
#' This function mirrors createTableOne, but allows for more customization.
#'
#' @param vars Character vector. The column names of variables to summarize.
#' @param varlables Character vector. The labels for variables in the returned table. Should be the same length as \code{var}.
#'

#' @export
#' @include prettytable_functions.R

table_one <- function(vars, varlabels=vars, data, strata, normal=NULL, exact=NULL,
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
                fun_n_prc=fun_n_prc,  fun_apprx_p=fun_apprx_p, fun_exact_p=fun_exact_p,
                fun_p_fmt = fun_p_fmt, measurelab_cat=measurelab_cat,sep=sep, nspaces=nspaces)
    )
  } else if(any(is_cat)) {
    tbl <-  cat_table(vars[is_cat], varlabels=varlabels[is_cat], data=data, strata=strata, exact=exact,
                      fun_n_prc=fun_n_prc,  fun_apprx_p=fun_apprx_p, fun_exact_p=fun_exact_p,
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

cat_table <- function(vars, varlabels=vars, data, strata, exact=NULL,
                      fun_n_prc=n_perc,  fun_apprx_p=p_cat_apprx, fun_exact_p=p_cat_exact,
                      fun_p_fmt = p_fmt, measurelab_cat="n (%)",sep=" -- ", nspaces=6,...) {
  ncols    = if(missing(strata)) 1 else nlevels(factor(data[[strata]]))  #ncols doesn't include the p column for now
  nvars    = length(vars)
  blank_row= rep("", ncols)
  spaces = paste(rep("&nbsp;", nspaces), collapse = "")

  if(missing(strata)) {
    #first deal with the simplest case -- missing strata.
    tbl      = t(t(unlist(lapply(1:nvars, function(i) c(blank_row, fun_n_prc(data[[vars[i]]]))))))
  } else {
    p_funs   = lapply(1:nvars, function(i) if(vars[i] %in% exact) p_cat_exact else p_cat_apprx)
    p_row    = t(sapply(1:nvars, function(i) c(blank_row, fun_p_fmt(p_funs[[i]](data[[vars[i]]], data[[strata]])))))
    list_tbls= lapply(1:nvars, function(i) rbind(p_row[i,], cbind(fun_n_prc(data[[vars[i]]], data[[strata]]),"")))
    tbl      = do.call(rbind, list_tbls)
    colnames(tbl)[ncol(tbl)] <- "p-value"
  }

  var_measure_labs <- paste0(varlabels, sep, measurelab_cat)

  rownames(tbl) = unlist(lapply(1:nvars, function(i) c(var_measure_labs[i],paste(spaces, levels(factor(data[[vars[i]]])) ))))
  tbl
}
