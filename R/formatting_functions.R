#' Formatting functions
#'
#' These are functions passed to either summary measure functions or fmt.glm_table. The idea is for you to be able to create your own modifications of them and pass them to the relevant functions, in order to fully customize the table.
#'


#regression / RR's etc
#' @export
beta_fmt = function(b, digits=2) format(as.numeric(b), digits=digits, nsmall=digits)
#' @export
#' @rdname beta_fmt
ci_beta_fmt = function(lwr, upr, sep=" to ", digits=2) paste0(beta_fmt(lwr, digits), sep, beta_fmt(upr, digits))
#' @export
#' @rdname beta_fmt
RR_fmt <- function(RR, digits=2) format(as.numeric(RR), digits=digits, nsmall=digits)
#' @export
#' @rdname beta_fmt
ci_RR_fmt <- function(lwr, upr, sep=" to ", digits=2) paste0(RR_fmt(lwr, digits), sep, RR_fmt(upr, digits))
#p values
#' @export
#' @rdname beta_fmt
p_fmt <- function(p, digits=3, eps=10^-digits) ifelse(p<eps, paste0("<",eps), formatC(format(p, digits=0, nsmall=digits), width=digits+3, format="f"))
#unvariate statistics
#' @export
#' @rdname beta_fmt
prop_fmt <- function(x, digits=1) paste0(" (",formatC(x*100, digits=digits, width = digits+3, format="f"),"%)")
#' @export
#' @rdname beta_fmt
sd_fmt <- function(x, digits=1) paste0("&plusmn;", mean_fmt(x, digits=digits))
#' @export
#' @rdname beta_fmt
mean_fmt <- function(x, digits=1) formatC(x, digits=digits, width=digits+3, format="f")
#' @export
#' @rdname beta_fmt
n_fmt <- function(x) {
  x[x!=""] = prettyNum(x[x!=""], big.mark=",")
  formatC(x, width=5, format="d") }
#' @export
#' @rdname beta_fmt
median_fmt <- function(x, digits=0) formatC(x, digits=digits, width=digits+3, format="f")
#' @export
#' @rdname beta_fmt
iqr_fmt <- function(lwr, upr, sep=" to ", bracket=c("[","]"), digits=0) paste0(bracket[1],median_fmt(lwr, digits=digits), sep, median_fmt(upr, digits=digits),bracket[2])
#' @export
#' @rdname beta_fmt
range_fmt <- iqr_fmt
