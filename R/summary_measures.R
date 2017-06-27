#' Functions to generate nice-looking summary measures in \code{table_one}
#'
#' @param x Variable to be summarized
#' @param ag.by Variable to aggregate by, usually a character or factor. Optional.
#' @param fun_n,fun_prop,fun_median,fun_iqr,fun_range,fun_mean,fun_sd Formatting functions passed as arguments.
#' @param margin Integer. 1=row percent, 2=column percent (default).
#' @param na.rm Logical. Remove NAs from calculation or return NA if present.

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

#' @rdname n_perc
median_iqr <- function(x, ag.by, fun_median=median_fmt, fun_iqr=iqr_fmt,na.rm=TRUE) {
  getMedIQR <- function(x) paste0(fun_median(median(x, na.rm=na.rm)),
                                  " ",
                                  fun_iqr(lwr=quantile(x, probs = .25, na.rm=na.rm),
                                          upr=quantile(x, probs = .75, na.rm=na.rm)))
  if(missing(ag.by)) result <- getMedIQR(x) else {
    result <- aggregate(x, list(ag.by), getMedIQR)$x
    names(result) <- aggregate(x, list(ag.by), getMedIQR)$Group.1
  }
  return(result)
}

#' @rdname n_perc
median_range <- function(x, ag.by, fun_median=median_fmt, fun_range=range_fmt, na.rm=TRUE) {
  getMedRange <- function(x) paste0(fun_median(median(x, na.rm=na.rm)),
                                    " ",
                                    fun_range(lwr=quantile(x, probs = 0, na.rm=na.rm),
                                              upr=quantile(x, probs = 1, na.rm=na.rm)))
  if(missing(ag.by)) result <- getMedRange(x) else {
    result <- aggregate(x, list(ag.by), getMedRange)$x
    names(result) <- aggregate(x, list(ag.by), getMedRange)$Group.1
  }
  return(result)
}

#' @rdname n_perc
mean_sd <- function(x, ag.by, fun_mean=mean_fmt, fun_sd=sd_fmt, na.rm=TRUE) {

  getMeanSD <- function(x) paste0(fun_mean(mean(x, na.rm=na.rm)), fun_sd(sd(x, na.rm=na.rm)))

  if(missing(ag.by)) result <- getMeanSD(x) else {
    result <- aggregate(x, list(ag.by), getMeanSD)$x
    names(result) <- aggregate(x, list(ag.by), getMeanSD)$Group.1
  }
  return(result)
}
