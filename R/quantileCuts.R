require(rpart)

qCut <- function(x, probs=seq(0,1,.25), labels=NULL) {
  cut(x, breaks=quantile(x, probs=probs, na.rm=TRUE), labels=labels)
}

getOptimalCuts <- function(..., cp=0.01) {
  tr <- rpart::prune(rpart::rpart(...), cp=cp)
  cuts <- c(0, tr$splits[,"index"], Inf)
  cuts
} 

getOptimalCuts.boot <- function(formula, data, cp=0.01, type="h", R=1000) {
  cuts <- function(formula, data, indices, cp=cp) {
    d <- data[indices,]
    tr <- rpart::prune(rpart::rpart(formula, data=d), cp=cp)
    cuts <- c(0, tr$splits[,"index"], Inf)
    cuts
  }
  if(type=="h") hist(boot::boot(data, cuts, R=R, formula=formula)) else {
    boot::boot(data, cuts, R=R, formula=formula)
  }
} 

boot.cutpoints <- function(data, formula, R=500, cp=0.02) {
  cuts <- function(formula, data, indices, cp=cp) {
    d <- data[indices,]
    tr <- rpart::prune(rpart::rpart(formula, data=d), cp=cp)
    cuts <- c(0, tr$splits[,"index"], Inf)
    cuts
  }
  boot.cuts <- function(data,formula) {
    d <- data[sample(1:nrow(data), size=nrow(data), replace=TRUE),]
    cuts(formula=mortality=="Alive" ~ Pulse, data=d, indices=, cp=cp)
  }
  boots <- replicate(R, boot.cuts(data, formula))
  lengths <- sapply(boots, length)
  do.call(rbind, boots[lengths==median(lengths)])
}


