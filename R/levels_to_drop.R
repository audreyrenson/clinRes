#' @export
#vector of levels of a variable in which n is less than min_n
#within levels of a strata.
#Useful for regression.

levels_to_drop <- function(varname, strata, data, min_n=10) {
  require(dplyr)

  reshape2::melt(table(data[,c(varname, strata)])) %>%
    group_by_(varname) %>%
    summarize(nmin = min(value)) %>%
    filter(nmin < min_n) %>%
    collect %>% .[[varname]]
}
