#' @export
prettytable_autoindent <- function(vars, data, nspaces=6, includeNA = TRUE,
                                   nonnormal=NULL, strata=NULL, ...) {
  #... args get passed to CreateTableOne


  if(!is.null(strata)){
    table1 <- print(tableone::CreateTableOne(vars=vars, data=data, includeNA = includeNA, strata=strata),
                    printToggle = FALSE, nonnormal=nonnormal,...)
  } else {
    table1 <- print(tableone::CreateTableOne(vars=vars, data=data, includeNA = includeNA),
                    printToggle = FALSE,...)
  }


  spaces = paste( rep("&nbsp;", nspaces), collapse="")

  varTypes <- data.frame(type = unlist(mosaic::dfapply(data[,vars], class)))
  varTypes$varname <- rownames(varTypes)


  varTypes$nrow <- sapply(seq_along(vars) , function(i) {
    if (varTypes$type[i] %in% c("factor","character")) {
      vect <- eval(parse(text=paste0("data$`", varTypes$varname[i], "`")))
      ifelse(length(levels(factor(vect,
                                  exclude=ifelse(includeNA, 9999, NA)))) == 2, 1,
             length(levels(factor(vect,
                                  exclude=ifelse(includeNA, 9999, NA)))) #using 9999 to represent excluding nothing
      )
    } else 1
  })
  varTypes$end <- cumsum(ifelse(varTypes$nrow == 1, 1, varTypes$nrow + 1))
  varTypes$start <- sapply(seq_along(varTypes$nrow),
                           function(i) ifelse(i==1, 1, varTypes$end[i-1] + 1)) + 1
  varTypes$end <- varTypes$end + 1
  varTypes$start_indenting <- ifelse(varTypes$nrow == 1, NA, varTypes$start + 1)


  index <- na.omit(unlist(
    sapply(seq_len(length(vars)),
           function(i)
             if (is.na(varTypes$start_indenting[i])) NA
           else seq(varTypes$start_indenting[i], varTypes$end[i]))
  ))


  rownames(table1)[index] <- paste0(spaces, rownames(table1)[index])
  table1

}
