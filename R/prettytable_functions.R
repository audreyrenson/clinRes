#' @export

# measures


n_perc <- function(x, ag.by, fun_n=n_fmt, fun_prop=prop_fmt, margin=2) {
  #unlike the other functions, this returns a vector if ag.by is missing, otherwise a matrix


  getNperc <- function(n, p) suppressWarnings(paste0(fun_n(n), fun_prop(p)))

  if(missing(ag.by)) {

    result <- getNperc(table(x), prop.table(table(x)))
    names(result) <- names(table(x))
  } else {

    n <- table(x, ag.by)
    p <- prop.table(n, margin)

    result <- sapply(1:ncol(n), function(i) getNperc(n[,i], p[,i]))
    colnames(result) <- colnames(n)
    rownames(result) <- rownames(n)
  }
  return(result)
}
median_iqr <- function(x, ag.by, fmt_median=median_fmt, fmt_iqr=iqr_fmt,na.rm=TRUE) {
  getMedIQR <- function(x) paste0(fmt_median(median(x, na.rm=na.rm)),
                                  " ",
                                  fmt_iqr(lwr=quantile(x, probs = .25, na.rm=na.rm),
                                          upr=quantile(x, probs = .75, na.rm=na.rm)))
  if(missing(ag.by)) result <- getMedIQR(x) else {
    result <- aggregate(x, list(ag.by), getMedIQR)$x
    names(result) <- aggregate(x, list(ag.by), getMedIQR)$Group.1
  }
  return(result)
}
median_range <- function(x, ag.by, fmt_median=median_fmt, fmt_range=range_fmt, na.rm=TRUE) {
  getMedRange <- function(x) paste0(fmt_median(median(x, na.rm=na.rm)),
                                    " ",
                                    fmt_range(lwr=quantile(x, probs = 0, na.rm=na.rm),
                                            upr=quantile(x, probs = 1, na.rm=na.rm)))
  if(missing(ag.by)) result <- getMedRange(x) else {
    result <- aggregate(x, list(ag.by), getMedRange)$x
    names(result) <- aggregate(x, list(ag.by), getMedRange)$Group.1
  }
  return(result)
}
mean_sd <- function(x, ag.by, fun_mean=mean_fmt, fun_sd=sd_fmt, na.rm=TRUE) {

  getMeanSD <- function(x) paste0(fun_mean(mean(x, na.rm=na.rm)), fun_sd(sd(x, na.rm=na.rm)))

  if(missing(ag.by)) result <- getMeanSD(x) else {
    result <- aggregate(x, list(ag.by), getMeanSD)$x
    names(result) <- aggregate(x, list(ag.by), getMeanSD)$Group.1
  }
  return(result)
}

# Formats

#regression / RR's etc
beta_fmt = function(b) format(as.numeric(b), digits=digits, nsmall=digits)
ci_beta_fmt = function(lwr, upr, sep=" to ") paste0(beta_fmt(lwr), sep, beta_fmt(upr))
RR_fmt <- function(RR) format(as.numeric(RR), digits=2, nsmall=2)
ci_RR_fmt <- function(lwr, upr, sep=" to ") paste0(RR_fmt(lwr), sep, RR_fmt(upr))
#p values
p_fmt <- function(p) format.pval(round(p,3), eps = .001)
#unvariate statistics
prop_fmt <- function(x) paste0(" (",formatC(x*100, digits=1, width = 4, format="f"),"%)")
sd_fmt <- function(x) paste0("±", formatC(x, digits=1, width=3, format="f"))
mean_fmt <- function(x) formatC(x, digits=1, width=4, format="f")
n_fmt <- function(x) formatC(x, width=5, format="d")
median_fmt <- function(x) formatC(x, digits=1, width=4, format="f")
iqr_fmt <- function(lwr, upr, sep=" to ", bracket=c("[","]")) paste0(bracket[1],median_fmt(lwr), sep, median_fmt(upr),bracket[2])
range_fmt <- iqr_fmt

# p_value calculations for table_one

p_cont_norm <- function(x,y) anova(lm(y~x))$`Pr(>F)`[1]
p_cont_nonnorm <- function(x,y) kruskal.test(y~x)$p.value
p_cat_exact <- function(x,y) fisher.test(x,y)$p.value
p_cat_apprx <- function(x,y) chisq.test(x,y)$p.value


# Tables

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
    tbl <- t(t(sapply(1:nvars, function(i) funs[[i]](data[,vars[i]]))  ))

  } else {
    #otherwise, it is a matrix with ncol=nlevels(strata) and an appropriate p value for var[i]
    tbl <- t(sapply(1:nvars, function(i) funs[[i]](data[,vars[i]], data[,strata] )  ))
    tbl <- cbind(tbl, p=sapply(1:nvars, function(i) fun_p_fmt(p_funs[[i]](data[,strata],data[,vars[i]]))))
  }
  rownames(tbl) <- paste0(varlabels, sep, var_measures) #rownames will be the same either way
  tbl
}

cat_table <- function(vars, varlabels=vars, data, strata, exact=NULL,
                      fun_n_prc=n_perc,  fun_apprx_p=p_cat_apprx, fun_exact_p=p_cat_exact,
                      fun_p_fmt = p_fmt, measurelab_cat="n (%)",sep=" -- ", nspaces=6,...) {
  ncols    = if(missing(strata)) 1 else nlevels(factor(data[,strata]))  #ncols doesn't include the p column for now
  nvars    = length(vars)
  blank_row= rep("", ncols)
  spaces = paste(rep("&nbsp;", nspaces), collapse = "")

  if(missing(strata)) {
    #first deal with the simplest case -- missing strata.
    tbl      = t(t(as.character(sapply(1:nvars, function(i) c(blank_row, fun_n_prc(data[,vars[i]]))))))
  } else {
    p_funs   = lapply(1:nvars, function(i) if(vars[i] %in% exact) p_cat_exact else p_cat_apprx)
    p_row    = t(sapply(1:nvars, function(i) c(blank_row, fun_p_fmt(p_funs[[i]](data[,vars[i]], data[,strata])))))
    list_tbls= lapply(1:nvars, function(i) rbind(p_row[i,], cbind(fun_n_prc(data[,vars[i]], data[,strata]),"")))
    tbl      = do.call(rbind, list_tbls)
    colnames(tbl)[ncol(tbl)] <- "p-value"
  }

  var_measure_labs <- paste0(varlabels, sep, measurelab_cat)

  rownames(tbl) = unlist(lapply(1:nvars, function(i) c(var_measure_labs[i],paste(spaces, levels(factor(data[,vars[i]])) ))))
  tbl
}


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


#cat_table(vars=c("race_ethnicity","GENDER"), apprx=c("race_ethnicity", "GENDER"), strata="race_ethnicity", data=final )




