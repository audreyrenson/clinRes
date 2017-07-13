table1_labels <- function(table1, varnames, varlabs, replace_equals=TRUE){
  
  for(i in seq_along(varlabs)) {
    rownames(table1) <- gsub(varnames[i], varlabs[i], rownames(table1))
  }
  
  if(replace_equals) rownames(table1) <- gsub(" = .*", " (%)", rownames(table1))
  table1
}

