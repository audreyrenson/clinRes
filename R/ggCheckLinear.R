#Examine functional relationship of a binary variable against a continuous variable


ggCheckLinear <- function(x, y, breaks=length(unique(x)) / 3, return_data=TRUE,
                          link = function(y) log(y/(1-y)),
                          ... #arguments to geom_smooth()
                          ) {
  library(ggplot2)

  x_quant <- unique(quantile(x, probs=seq(0,1,length.out = breaks), na.rm=TRUE))
  x_cut   <- cut(x, breaks=x_quant)
  plot_tbl <- prop.table(table(x_cut,y), 1)
  plot_dat <- data.frame(index   = 1:nrow(plot_tbl),
                         x_quant = rownames(plot_tbl),
                         y       = link(plot_tbl[,2]))

  print(ggplot(plot_dat, aes(x=index, y=y)) +
    geom_point() +
    geom_smooth(...))
  if(return_data) rownames(plot_dat) <- NULL; invisible(plot_dat)
}


