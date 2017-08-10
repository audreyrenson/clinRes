#' ggplot2 for ROC curves
#'
#' Plot one or more ROC curves using the ggplot2 interface.
#'
#' @param list_rocs A list of objects returned by \code{pROC::roc(ci=TRUE)}.
#' @param fmt A function receiving the parameters \code{roc} and \code{digits}, to return the AUC text in the annotation.
#' @param labs Character vector, same length as list_rocs, with labels for the different models.
#' @param roc An object returned by \code{pROC::roc(ci=TRUE)}.
#' @param digits Integer vector of length 1. The number of digits to print for AUC and 95% confidence interval.

#' @export
ggroc <- function(list_rocs, fmt=fmt_auc, labs) {

 dat <- do.call(rbind, lapply(seq_along(list_rocs), function(i)
    data.frame(Sensitivity=list_rocs[[i]]$sensitivities,
               Specificity=list_rocs[[i]]$specificities,
               lab=paste(labs[i], fmt(list_rocs[[i]])))))


  ggplot(data= dat, aes(Specificity, Sensitivity, color=lab)) +
    geom_line(size=1) +
    scale_x_reverse() +
    # geom_rect(xmin=0.04,ymin=0, xmax=-.78,ymax=.12,color="black",fill=NA) +
    # annotate(geom="text", x=.36, y=0.07, label=fmt_auc(roc1)) +
    theme_light() +
    theme(legend.title = element_blank(),
          legend.position = c(0.63, 0.15),
          legend.box.background = element_rect(color="darkgrey",linetype=1)) +
    guides(color=guide_legend(ncol=1)) +
    geom_abline(slope=1, intercept = 1, color="grey")

}
#' @export
fmt_auc <- function(roc, digits=2) {
  fmt <- function(x) round(x,digits)
  res <- paste0(fmt(roc$auc), " [", fmt(roc$ci[1]), "-",fmt(roc$ci)[3], "]")
  if(for_plot) res <- paste0("AUC [95% CI]: ", res)
  res
}
