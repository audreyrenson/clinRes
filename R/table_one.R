#' Produce formatted descriptives tables
#'
#' Theses functions mirror createTableOne, but allow for more customization.
#'
#' @param x,y Variables for univariate p-value calculation. Within \code{table_one}, these work out to be \code{x=data[[strata]], y=data[[vars[i]]]}.
#' @param vars Character vector. The column names of variables to summarize.
#' @param varlables Character vector. The labels for variables in the returned table. Should be the same length as \code{var}.
#' @param data Data.frame in which to look for variables.
#' @param strata Character. Stratifying variable.
#' @param digits Integer. Number of digits passed to formatting function except quantile and p-value formats (see below).
#' @param q.digits Integer. Number of digits passed to fun_nonnorm (the default of which is median_iqr())
#' @param p.digits Integer. Number of digits passed to p_fmt.
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
#' @param test Logical. Include hypothesis tests in a stratified table?
#'
#' @include summary_measures.R
#' @include formatting_functions.R

#' @export
table_one <- function(vars=names(data), varlabels=vars, data, strata, normal=NULL, exact=NULL,
                      all_levels=FALSE, digits=1, q.digits=0, p.digits=3,
                      fun_n_prc=n_perc,  fun_apprx_p=p_cat_apprx, fun_exact_p=p_cat_exact,
                      fun_norm=mean_sd, fun_nonnorm=median_iqr,  fun_norm_p=p_cont_norm,
                      fun_nonnorm_p = p_cont_nonnorm, fun_p_fmt = p_fmt, fun_n_fmt = n_fmt,
                      measurelab_nonnormal=", median [IQR]", measurelab_normal=", mean&plusmn;SD",
                      measurelab_cat=" (%)", sep="", nspaces=6, header=NULL, groups=NULL,
                      includeNA=NULL, NAlabel="Missing (%)", include_overall=FALSE, test=TRUE) {
  #checks
  checkvars = if(missing(strata)) vars else c(strata, vars)
  if(!all(checkvars %in% names(data)))
      stop(paste0("Variable '", paste(checkvars[!checkvars %in% names(data)], collapse = ", "), "' not in dataset."))
  if(length(vars) != length(varlabels)) stop("vars and varlabels must be the same length.")


  #first, get total row
  Total      = fun_n_fmt( if(missing(strata)) nrow(data) else n_perc(data[[strata]], digits=digits))
  Total      = if(test & !missing(strata)) cbind(rbind(Total), "P-value"="") else Total

  #figure out which are categorical
  is_cat    = sapply(data[vars], function(i) class(i) %in% c("logical","character","factor"))

  #convert all categorical to factor
  data[,vars[is_cat]] = lapply(data[,vars[is_cat]], factor)
  #including strata
  if(!missing(strata)) data[[strata]] = factor(data[[strata]])

  #set up table
  list_tbls <- vector("list")

  for(i in 1:length(vars)) {
    if(is_cat[i]) {
      list_tbls[[i]] <- cat_table(
        varname = vars[i],
        varlabel = varlabels[i],
        data = data,
        strata = strata,
        all_levels = all_levels,
        fun_format = fun_n_prc,
        fun_p = if(vars[i] %in% exact) p_cat_exact else p_cat_apprx,
        fun_p_fmt=fun_p_fmt,
        measurelab=measurelab_cat,
        sep=sep, nspaces=nspaces,  header=header,
        includeNA="cat" %in% includeNA,
        NAlabel=NAlabel,
        test=test,
        digits=digits,
        p.digits=p.digits
      )
    } else {
      list_tbls[[i]] <- cont_table(
        varname = vars[i],
        varlabel = varlabels[i],
        data = data,
        strata = strata,
        fun_format = if(vars[i] %in% normal) fun_norm else fun_nonnorm,
        fun_p = if(vars[i] %in% normal) p_cont_norm else p_cont_nonnorm,
        fun_p_fmt=fun_p_fmt,
        measurelab=if(vars[i] %in% normal) measurelab_normal else measurelab_nonnormal,
        sep=sep, nspaces=nspaces,  header=header,
        includeNA="cont" %in% includeNA,
        NAlabel=NAlabel,
        test=test,
        digits=if(vars[i] %in% normal) digits else q.digits,
        p.digits=p.digits
      )
    }

  }

  tbl <- rbind(Total, do.call(rbind, list_tbls))

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
    clnms <- c("Overall", colnames(tbl))
    tbl <- cbind(table_one(vars=vars, varlabels=varlabels, data=data, normal=normal,
                           exact=exact, all_levels=all_levels, fun_n_prc=fun_n_prc,
                           fun_apprx_p=fun_apprx_p, fun_exact_p=fun_exact_p,
                           fun_norm=fun_norm, fun_nonnorm=fun_nonnorm,  fun_norm_p=fun_norm_p,
                           fun_nonnorm_p = fun_nonnorm_p, fun_p_fmt = fun_p_fmt,
                           fun_n_fmt = fun_n_fmt, measurelab_nonnormal=measurelab_nonnormal,
                           measurelab_normal=measurelab_normal, measurelab_cat=measurelab_cat,
                           sep=sep, nspaces=nspaces, header=header, groups=groups,
                           includeNA=includeNA, NAlabel=NAlabel)$table,
                 tbl)
    colnames(tbl) <- clnms
  }

  tbl <- list(table=tbl)
  if(!missing(strata)) {
    tbl[["strata"]] <- strata
    tbl[["include_overall"]] <- include_overall
    tbl[["strata_nlevs"]] <- nlevels(data[[strata]])
  }
  class(tbl) <- c("table_one", class(tbl))
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

#S3 method for htmlTable
htmlTable.table_one <- function(x, caption="Descriptive statistics.", ...) {
  list_args <- list(x = x$table, caption=caption, ...)
  if("strata" %in% names(x)) {
    list_args[["cgroup"]] <- x$strata; list_args[["n.group"]] <- x$strata_nlevs + 1
    if(x$include_overall) {
      list_args$cgroup <- c("", list_args$cgroup); list_args$n.cgroup <- c(1, list_args$n.cgroup)
    }
  }

  do.call(htmlTable, list_args)
}
#S3 method for kable
#' @export
kable <- function(x) UseMethod("kable", x)
#' @export
kable.default <- function(x, ...) knitr::kable(x,...)
#' @export
kable.table_one <- function(x, caption="Descriptive statistics.", escape=FALSE, ...) {
  tbl_print <- knitr::kable(x$table, caption=caption, format="html",
                            escape=escape,...)
  if("strata" %in% names(x)) {
    header = c(1, x$strata_nlevs + 1)
    names(header) = c(" ",  x$strata)
    if(x$include_overall) {
      header[" "] <- 2
    }

    kableExtra::add_header_above(tbl_print, header=header)
  } else tbl_print
}


