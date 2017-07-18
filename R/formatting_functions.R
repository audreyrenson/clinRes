#' Formatting functions
#'
#' These are functions passed to either summary measure functions or fmt.glm_table. The idea is for you to be able to create your own modifications of them and pass them to the relevant functions, in order to fully customize the table.
#'


#regression / RR's etc
#' @export
beta_fmt = function(b) format(as.numeric(b), digits=digits, nsmall=digits)
#' @export
#' @rdname beta_fmt
ci_beta_fmt = function(lwr, upr, sep=" to ") paste0(beta_fmt(lwr), sep, beta_fmt(upr))
#' @export
#' @rdname beta_fmt
RR_fmt <- function(RR) format(as.numeric(RR), digits=2, nsmall=2)
#' @export
#' @rdname beta_fmt
ci_RR_fmt <- function(lwr, upr, sep=" to ") paste0(RR_fmt(lwr), sep, RR_fmt(upr))
#p values
#' @export
#' @rdname beta_fmt
p_fmt <- function(p, eps=.001, digits=3) if(p<eps) paste0("<",eps) else format(p, digits=digits, nsmall=digits)
#unvariate statistics
#' @export
#' @rdname beta_fmt
prop_fmt <- function(x) paste0(" (",formatC(x*100, digits=1, width = 4, format="f"),"%)")
#' @export
#' @rdname beta_fmt
sd_fmt <- function(x) paste0("&plusmn;", mean_fmt(x))
#' @export
#' @rdname beta_fmt
mean_fmt <- function(x) formatC(x, digits=1, width=4, format="f")
#' @export
#' @rdname beta_fmt
n_fmt <- function(x) formatC(x, width=5, format="d")
#' @export
#' @rdname beta_fmt
median_fmt <- function(x) formatC(x, digits=1, width=4, format="f")
#' @export
#' @rdname beta_fmt
iqr_fmt <- function(lwr, upr, sep=" to ", bracket=c("[","]")) paste0(bracket[1],median_fmt(lwr), sep, median_fmt(upr),bracket[2])
#' @export
#' @rdname beta_fmt
range_fmt <- iqr_fmt
