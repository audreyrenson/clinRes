#' @export
#filter a variable so that levels with fewer than 'min_n' observations
#within strata of 'strata' variable get assigned NA and dropped.

filter_levels <- function(varname, strata, data, min_n=10) {
  require(dplyr)

  low_freq_levels <- reshape2::melt(table(data[,c(varname, strata)])) %>%
    group_by_(varname) %>%
    summarize(nmin = min(value)) %>%
    filter(nmin < min_n) %>%
    collect %>% .[[varname]]

  result <- data[, varname]
  result[result %in% low_freq_levels] <- NA
  result <- droplevels(result)

  return(result)
}

