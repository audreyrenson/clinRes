#' Produce formatted descriptives tables
#'
#' Theses functions mirror createTableOne, but allow for more customization.
#'
#' @param x,y Variables for univariate p-value calculation. Within \code{table_one}, these work out to be \code{x=data[[strata]], y=data[[vars[i]]]}.
#' @param vars Character vector. The column names of variables to summarize.
#' @param varlables Character vector. The labels for variables in the returned table. Should be the same length as \code{var}.
#' @param data Data.frame in which to look for variables.
#' @param strata Character. Stratifying variable.
#' @param normal Character vector. Variables to treat as normally distributed. If not included in this, numeric variables are considered non-normal.
#' @param exact Character vector. Variable to perform exact tests on.
#' @param fun_n_prc,fun_norm,fun_nonnorm Summary measure functions.
#' @param fun_apprx_p,fun_exact_p,fun_norm_p,fun_nonnorm_p P-value functions.
#' @param fun_p_fm Formatting function for p-values.
#' @param measurelab_nonnormal,measurelab_normal,measurelab_cat Character. Text to put in the row indicating how the variable is summarized.
#' @param sep Character. Text to separate the variable label from the measure label.
#' @param nspaces Integer. Number of spaces to indent factor levels.
#' @param groups List of vectors whose elements are the names of variables in each group, and the list names are the names of the groups.
#' @param includeNA Character. Either "cont","cat", or c("cont","cat"). Whether or not to show the n (%) of NA's for continuous and categorical variables, respectively.
#' @param NAlabel Character. Row label for NA rows.
#' @param include_overall Logical. Include non-stratified column in stratified table? Default FALSE.
#' @include summary_measures.R
#' @include formatting_functions.R

#' @export
table_one <- function(vars=names(data), varlabels=vars, data, strata, normal=NULL, exact=NULL, all_levels=FALSE,
                      fun_n_prc=n_perc,  fun_apprx_p=p_cat_apprx, fun_exact_p=p_cat_exact,
                      fun_norm=mean_sd, fun_nonnorm=median_iqr,  fun_norm_p=p_cont_norm,
                      fun_nonnorm_p = p_cont_nonnorm, fun_p_fmt = p_fmt, fun_n_fmt = n_fmt,
                      measurelab_nonnormal=", median [IQR]", measurelab_normal=", mean&plusmn;SD",
                      measurelab_cat=" (%)", sep="", nspaces=6, header=NULL, groups=NULL,
                      includeNA=NULL, NAlabel="Missing (%)", include_overall=FALSE) {
  #checks
  checkvars = if(missing(strata)) vars else c(strata, vars)
  if(!all(checkvars %in% names(data)))
      stop(paste0("Variable '", paste(checkvars[!checkvars %in% names(data)], collapse = ", "), "' not in dataset."))
  if(length(vars) != length(varlabels)) stop("vars and varlabels must be the same length.")


  #first, get total row
  Total      = fun_n_fmt( if(missing(strata)) nrow(data) else c( n_perc(data[[strata]]), "P-value"="") )

  is_cat    = sapply(data[vars], function(i) class(i) %in% c("logical","character","factor"))

  #convert all categorical to factor
  data[,vars[is_cat]] = lapply(data[,vars[is_cat]], factor)
  #including strata
  if(!missing(strata)) data[[strata]] = factor(data[[strata]])
  #include NA as a level if includeNA = TRUE
  if("cat" %in% includeNA) data[,vars[is_cat]] = lapply(data[,vars[is_cat]], addNAlevel, NAlabel=NAlabel)

  tbl_cont <- if(any(!is_cat)) cont_table(vars=vars[!is_cat], varlabels=varlabels[!is_cat], data=data, strata=strata, normal=normal,
                                         fun_norm=fun_norm, fun_nonnorm=fun_nonnorm,  fun_norm_p=fun_norm_p,
                                         fun_nonnorm_p = fun_nonnorm_p, fun_p_fmt = fun_p_fmt,
                                         measurelab_nonnormal=measurelab_nonnormal, nspaces=nspaces,
                                         measurelab_normal=measurelab_normal, sep=sep, header=header,
                                         includeNA="cont" %in% includeNA, NAlabel=NAlabel) else NULL
  tbl_cat <- if(any(is_cat)) cat_table(vars=vars[is_cat], varlabels=varlabels[is_cat], data=data, strata=strata, exact=exact,
                                        all_levels=all_levels, fun_n_prc=fun_n_prc,  fun_apprx_p=fun_apprx_p, fun_exact_p=fun_exact_p,
                                        fun_p_fmt = fun_p_fmt, measurelab_cat=measurelab_cat,sep=sep, nspaces=nspaces,
                                        header=header) else NULL

  tbl <- rbind(Total, tbl_cont, tbl_cat)

  #add row groupings if specified
  if(!is.null(groups)) {
    for(i in 1:length(groups)) {
      varlabs_gp <- varlabels[vars %in% groups[[i]]]
      group_index <- unlist(sapply(varlabs_gp, function(j) grep(j, rownames(tbl))))
      group_measure_lab <-  if(groups[[i]][1] %in% vars[is_cat]) measurelab_cat else if(groups[[i]][1] %in% normal) measurelab_normal else measurelab_nonnormal
      group_lab <- paste0(names(groups)[i], group_measure_lab)
      #first add the indents
      tbl <- table_indents(tbl, index=group_index, nspaces = nspaces)
      #add the blank group title row
      tbl <- rbind(tbl[1:(min(group_index)-1),,drop=FALSE],
                    "",
                    tbl[min(group_index):nrow(tbl),,drop=FALSE])
      #name the blank row
      rownames(tbl)[min(group_index)] <- group_lab
      #remove measure labs from grouped rows (is now +1 because added group title row)
      gsub_measure_lab <- gsub("\\(","\\\\(", group_measure_lab)
      rownames(tbl)[group_index + 1] <- gsub(gsub_measure_lab, "", rownames(tbl)[group_index + 1])
    }
  }

  if(include_overall & !missing(strata)) {
    #append a non-stratified table to the left-hand side
    listargs <- as.list(sys.call())
    listargs[[strata]] <- NULL
    tbl <- cbind(Overall = do.call(table_one, listargs), tbl)
  }
  tbl
}


#' @export
#' @rdname table_one
cont_table <- function(vars, varlabels=vars, data, strata, normal=NULL,
                       fun_norm=mean_sd, fun_nonnorm=median_iqr,  fun_norm_p=p_cont_norm,
                       fun_nonnorm_p = p_cont_nonnorm, fun_p_fmt = p_fmt,
                       measurelab_nonnormal=", median [IQR]",
                       measurelab_normal=", mean&plusmn;SD", sep="",
                       nspaces=6,  header=NULL,
                       includeNA=TRUE, NAlabel="Missing (%)", ...) {

  nvars <- length(vars)
  funs <- lapply(vars, function(i) if(i %in% normal) fun_norm else fun_nonnorm)  #get a list of measure functions to go with each variable
  p_funs <- lapply(vars, function(i) if(i %in% normal) fun_norm_p else fun_nonnorm_p) #get a list of p functions
  var_measures <- ifelse(vars %in% normal , measurelab_normal, measurelab_nonnormal) #vector of measure label functions to paste onto the rownames

  spaces = paste(rep("&nbsp;", nspaces), collapse = "") #this is only applicable to the NA rows

  if(missing(strata)) {
    #if there is no strata variable, this is just one column and no p-value
    tbl <- do.call(rbind, lapply(1:nvars, function(i)
      rbind(
        funs[[i]](data[[ vars[i] ]]), # <- this creates the row of the actual measure(s) (/and p-value)
        getNAs(data[[ vars[i] ]]) # <- this creates the NA row
      )))

    colnames(tbl) =  if(is.null(header)) "Overall" else header

  } else {
    #otherwise, it is a matrix with ncol=nlevels(strata) and an appropriate p value for var[i]
    tbl <- do.call(rbind, lapply(1:nvars, function(i)
      rbind(
        funs[[i]](data[[ vars[i] ]], data[[ strata ]] ),
        getNAs(x = data[[ vars[i] ]], data[[ strata]])
      )))
    tbl <- cbind(tbl, p=c(sapply(1:nvars, function(i) c(fun_p_fmt(p_funs[[i]](data[[strata]],data[[vars[i]]])),""))))
    colnames(tbl)[ncol(tbl)] <- "P-value"
    if(!is.null(header)) colnames(tbl) = header
  }
  rownames(tbl) <- c(rbind(                   #rownames will be the same either way.
    paste0(varlabels, sep, var_measures),
    paste0(spaces, NAlabel))) #The c(rbind()) business is to intersperse "" for the NA rows.

  if(includeNA==FALSE) tbl <- t(t(odd(tbl)))

  if(length(vars)==1) tbl <- t(tbl) # <- this is because it will return a vector if length=1 which messes up rbind()
  tbl
}

#' @export
#' @rdname table_one
cat_table <- function(vars, varlabels=vars, data, strata, all_levels=FALSE, exact=NULL,
                      fun_n_prc=n_perc,  fun_apprx_p=p_cat_apprx, fun_exact_p=p_cat_exact,
                      fun_p_fmt = p_fmt, measurelab_cat=" (%)",sep="", nspaces=6,header=NULL,...) {
  ncols    = if(missing(strata)) 1 else nlevels(data[[strata]])  #ncols doesn't include the p column for now
  nvars    = length(vars)
  nlevs    = sapply(vars, function(i) nlevels(data[[i]]))
  blank_row= rep("", ncols)
  spaces = paste(rep("&nbsp;", nspaces), collapse = "")

  if(missing(strata)) {
    #first deal with the simplest case -- missing strata.
    tbl      = t(t(unlist(lapply(1:nvars, function(i)
      if(nlevs[i]==2 & !all_levels) fun_n_prc(data[[vars[i]]])[2] else c(blank_row, fun_n_prc(data[[vars[i]]]))))))
    colnames(tbl) = if(is.null(header)) "Overall" else header
  } else {

    p_funs   = lapply(1:nvars, function(i) if(vars[i] %in% exact) p_cat_exact else p_cat_apprx)
    p_vals    = sapply(1:nvars, function(i) fun_p_fmt(p_funs[[i]](data[[vars[i]]], data[[strata]])))
    p_row    = t(sapply(1:nvars, function(i) c(blank_row, p_vals[i])))
    list_tbls = vector("list", nvars)
    for(i in 1:nvars) {
      if(nlevs[i]==2 & !all_levels) {
        list_tbls[[i]] <- c(fun_n_prc(data[[vars[i]]], data[[strata]])[2,], p_vals[i])
      } else {
        list_tbls[[i]] <- rbind(p_row[i,], cbind(fun_n_prc(data[[vars[i]]], data[[strata]]),""))
      }
    }
      tbl      = do.call(rbind, list_tbls)
      colnames(tbl)[ncol(tbl)] <- "P-value"
      if(!is.null(header)) colnames(tbl) = header

  }

  var_measure_labs <- sapply(1:nvars, function(i)
    if(nlevs[i]==2 & !all_levels) paste0(varlabels[i], "=", levels(factor(data[[vars[i]]]))[2], sep, measurelab_cat)
    else paste0(varlabels[i], sep, measurelab_cat))

  rownames(tbl) = unlist(lapply(1:nvars, function(i)
    if(nlevs[i]==2 & !all_levels)  paste(var_measure_labs[i]) else c(var_measure_labs[i],paste(
    spaces, levels(data[[vars[i]]]) ))))

  colnames(tbl) = header

  tbl
}

#' @export
#' @rdname table_one
#' @param  incides List of indices representing the row groups.
#' @param  labels  Headings for row groups.
#' @param  tbl     Matrix object returned by \code{table_one()}.
table_one.rgroup <- function(tbl, indices, labels, nspaces=6) {
  for(i in seq_along(labels)) {
    tbl <- miscTools::insertRow(m=tbl, r = indices[[i]][1], v = "", rName = labels[i])
    tbl <- table_indents(tbl, index=indices[[i]]+1, nspaces=nspaces)
    if(i<length(labels))  indices[[i+1]] <- indices[[i+1]] + 1
  }
  tbl
}

#' @export
#' @rdname table_one
p_cont_norm <- function(x,y) anova(lm(y~x))$`Pr(>F)`[1]
#' @export
#' @rdname table_one
p_cont_nonnorm <- function(x,y) kruskal.test(x,y)$p.value
#' @export
#' @rdname table_one
p_cat_exact <- function(x,y) fisher.test(x,y)$p.value
#' @export
#' @rdname table_one
p_cat_apprx <- function(x,y) chisq.test(x,y)$p.value

#functions to get NAs for cont_table
getNAs <- function(x, strata) {
  if(any(is.na(x))) {
    if(missing(strata)) n_perc(is.na(x))["TRUE"] else n_perc(is.na(x), strata)["TRUE",]
  } else {
    if(missing(strata)) strata=1
    getNoNAs_n_perc(strata)
  }
}
#this is probably not ideal, but solves the problem that when there are no NAs, getNAs was returning an error.
getNoNAs_n_perc <- function(strata) {
  rep("    0 ( 0.0%)", nlevels(factor(strata)))
}
#function to get odd numbered rows of table to exclude NA rows
odd <- function(tbl) tbl[ which( (1:nrow(tbl) %% 2) == 1), ]
#function to add missing as a level
addNAlevel <- function(x, NAlabel, ifany=TRUE) {
  z <- addNA(factor(x), ifany = ifany)
  levels(z)[is.na(levels(z))] <- NAlabel
  z
}
