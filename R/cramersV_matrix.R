#' @export
cramersV_matrix <- function(data) {
  mtrx <- sapply(1:ncol(data), function(var1)
    sapply(1:ncol(data), function(var2)
      lsr::cramersV(data[,var1], data[,var2])))
  diag(mtrx) <- 1
  rownames(mtrx) <- colnames(mtrx) <- colnames(data)
  mtrx
}
