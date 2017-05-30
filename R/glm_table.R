#' @export
glm_table <- function(fit, measure="OR") {
  # this will give you a neatly formatted table with exponentiated betas
  # from a glm object. Note: does not work with interaction terms
  options(scipen=9999)

  if(class(fit) != "zeroinfl") {

    pvals <- ifelse(coef(summary(fit))[,4] < 0.0001, "< 0.0001",
                    as.character(round(coef(summary(fit))[,4], digits=4)))
    df <- data.frame(
      round(exp(coef(fit)), digits=3),
      round(exp(confint(fit)),digits=3),
      pvals
    )

  } else if(fit$dist == "negbin") {

    pvals_count <- ifelse(summary(fit)$coefficients$count[,4] < 0.0001, "< 0.0001",
                          as.character(round(summary(fit)$coefficients$count[,4], digits=4)))
    pvals_zero <- ifelse(summary(fit)$coefficients$zero[,4] < 0.0001, "< 0.0001",
                         as.character(round(summary(fit)$coefficients$zero[,4], digits=4)))

    df <- data.frame(
      round(exp(coef(fit)), digits=3),
      round(exp(confint(fit)),digits=3),
      c(pvals_count[1:length(pvals_count)-1], pvals_zero)
    )
  }
  colnames(df) <- c(measure,"95%","CI   ","p-value")

  df
}

