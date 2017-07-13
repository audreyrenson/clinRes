roundCoefs <- function(data, indices, fit, mult=2, fun=round, eval.type="auc", y) {
  coefs <- coef(fit)[-1]
  beta_score <- fun(mult*coefs)
  pred <-  as.numeric(data[indices,] %*% beta_score)
  roc_score <- pROC::roc(y[indices], pred, ci=TRUE)
  if(eval.type=="auc") roc_score$auc
}



boot.roundCoefs <- function(data, y, fit, mult.seq=seq(.25, 5, .25), fun=round, eval.type="auc",R=1000) {
  tbl <- sapply(mult.seq, function(i)
      replicate(n = R, expr = roundCoefs(data, indices=sample(1:nrow(data), size=nrow(data), replace = TRUE),   
                                     fit, mult=i, fun=round, eval.type="auc", y))
  )
  colnames(tbl) = mult.seq
  
  dat <- reshape2::melt(tbl) %>%
    group_by(Var2) %>%
    mutate(median=median(value))
  
  res <- list(boots = tbl, max_median = max(dat$median), max_mult = min(dat$Var2[dat$median==max(dat$median)]))
  
}



plot_score_aucs <- function(boot.dat) {
  
  dat <- reshape2::melt(boot.dat$boots) %>%
    group_by(Var2) %>%
    mutate(median=median(value)) %>%
    ungroup() %>% 
    mutate(max_median=median==max(median))
  
  
  dat %>%
    ggplot(aes(x=factor(Var2), y=value, color=max_median)) +
    geom_boxplot() + 
    theme_minimal() +
    theme(legend.position = "none") +
    labs(y="AUC",x="Mutiple")
}





