#' Fit linear model
#'
#' @param y Proportion of the disease. Must be a value between 0 and 1.
#' @param time Measurement time
#'
#' @return r Growth rate
#' @return y0 Initial inoculum
#' @return time Measurement time
#' @return prev Predicted data
#' @return r2 Determination coefficient between the observed and predicted data
#'
#' @importFrom magrittr %>%
#' @importFrom stats lm
#'
#' @examples
#' dap <- seq(10, 60, 10)
#' inc <- c(0.18, 0.30, 0.40, 0.60, 0.90, 0.98)
#' lin(y = inc , time = dap)


#' @export
lin <- function(y, time) {

  fit.lin <- lm(y ~ time)
  r <- fit.lin$coefficients[2] %>% unname()
  y0 <- fit.lin$coefficients[1] %>% unname()
  prev <- fit.lin$fitted.values  #previstos destransformados
  r2 <- summary(lm(prev ~ y))$r.squared #ver o coeficiente R2

  return(list(
    r = r,
    y0 = y0,
    time = time,
    prev = unname(prev),
    r2 = r2
  ))

}
