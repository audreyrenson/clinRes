#' @export
table_indents <- function(tbl, index, nspaces=6) {
  #given a vector of indices, this will add html indents to the rownames
  spaces = paste( rep("&nbsp;", nspaces), collapse="")
  rownames(tbl)[index] <- paste0(spaces, rownames(tbl)[index])
  tbl
}
