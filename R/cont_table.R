#let these expect one variable
cont_table <- function(varname, varlabel=varname, data, strata,
                       fun_format = mean_sd, fun_p = p_cont_norm,
                       fun_p_fmt = p_fmt, measurelab = ", mean&plusmn;SD",
                       sep="", nspaces=6,  header=NULL,
                       includeNA=TRUE, NAlabel="Missing (%)", test=TRUE, ...) {

  spaces = paste(rep("&nbsp;", nspaces), collapse = "") #this is only applicable to the NA rows

  if(missing(strata)) {
    #if there is no strata variable, this is just one column and no p-value
    tbl <- rbind(
        fun_format(data[[ varname ]]), # <- this creates the row of the actual measure(s) (/and p-value)
        getNAs(data[[ varname ]]) # <- this creates the NA row
    )

    colnames(tbl) =  if(is.null(header)) "Overall" else header

  } else {
    #otherwise, it is a matrix with ncol=nlevels(strata) - bones of table to be modified:
    tbl <-
      rbind(
        fun_format(data[[ varname ]], data[[ strata ]] ),
        getNAs(x = data[[ varname ]], data[[ strata]])
      )

    if(test) { # and add p value if requested
      tbl <- cbind(tbl, "P-value"=c(fun_p_fmt(fun_p(data[[strata]],data[[varname]])),""))
    }
    if(!is.null(header)) colnames(tbl) = header
  }
  rownames(tbl) <- c(rbind(                   #rownames will be the same either way.
    paste0(varlabel, sep, measurelab),
    paste0(spaces, NAlabel))) #The c(rbind()) business is to intersperse "" for the NA rows.

  if(includeNA==FALSE) tbl <- matrix(tbl[1,], nrow=1, dimnames=list(rownames(tbl)[1], colnames(tbl)))

  tbl
}
