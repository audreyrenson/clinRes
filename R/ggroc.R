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
