cat_table <- function(varname, varlabel=varname, data, strata, all_levels=FALSE,
                      fun_format = n_perc, fun_p = p_cat_apprx,
                      fun_p_fmt = p_fmt, measurelab=" (%)",
                      sep="", nspaces=6,  header=NULL,
                      includeNA=TRUE, NAlabel="Missing (%)",test=TRUE,
                      digits=1, p.digits=3, ...) {

  ncols    = if(missing(strata)) 1 else nlevels(data[[strata]])  #ncols doesn't include the p column for now
  nlevs    = nlevels(data[[varname]])
  blank_row= rep("", ncols)
  spaces = paste(rep("&nbsp;", nspaces), collapse = "")

  if(includeNA & any(is.na(data[[varname]]))) {
    data[[varname]] = addNAlevel(data[[varname]], NAlabel=NAlabel)
    all_levels=TRUE # <- currently have to set all_levels=TRUE to have the missing row show up.
  }

  if(missing(strata)) {
    #first deal with the simplest case -- missing strata.
    tbl      = if(nlevs==2 & !all_levels) c(fun_format(data[[varname]], digits=digits)[2]) else c(blank_row, fun_format(data[[varname]], digits=digits))
    tbl      = matrix(tbl, ncol=ncols, dimnames=list(names(tbl),
                                                     if(is.null(header)) "Overall" else header))
  } else {

    #set up the bones of the stratified table to be modified as needed
    tbl <- cbind(fun_format(data[[varname]], data[[strata]], digits=digits))
    if(nlevs==2 & !all_levels) tbl <- tbl[2, , drop=FALSE]

    if(test) { #add p-values
      p_x      = data[[varname]] [!data[[varname]]==NAlabel] # <- drop NAs from the p_value calculation
      p_y      = data[[strata]] [!data[[varname]]==NAlabel] # <- drop NAs from the p_value calculation
      p_val    = fun_p_fmt(fun_p(p_x, p_y), digits=p.digits)
      p_row    = c(blank_row, p_val)

      if(nlevs==2 & !all_levels) {
        tbl <- cbind(tbl, p_val)
      } else {
        tbl <- rbind(p_row, cbind(tbl,""))
      }

      colnames(tbl)[ncol(tbl)] <- "P-value"

    } else { #or not

      #this just adds a blank row for the variable label to a multi-row group,
      #when not using a p-value

      if(nlevs > 2 | all_levels) {
        tbl <- rbind("", tbl)
      }

    }


    if(!is.null(header)) colnames(tbl) = header

  }

  var_measure_lab <-  if(nlevs==2 & !all_levels) {
    paste0(varlabel, "=", levels(factor(data[[varname]]))[2], sep, measurelab)
  } else paste0(varlabel, sep, measurelab)

  rownames(tbl) =  if(nlevs==2 & !all_levels) var_measure_lab else c(var_measure_lab,paste(
      spaces, levels(data[[varname]]) ))

  tbl
}
