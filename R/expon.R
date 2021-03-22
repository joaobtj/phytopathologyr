#' Fit Exponential model
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
#' dap <- seq(1, 70, 10)
#' inc <- c(0.01, 0.04, 0.05, 0.11, 0.30, 0.55, 0.94)
#' expon(y=inc, time=dap)


#' @export
expon <- function(y, time) {

  exponit <- log(y) #transforma a variável
  fit.expon <- lm(exponit ~ time)
  r <- fit.expon$coefficients[2] %>% unname()
  exponit0 <- fit.expon$coefficients[1] %>% unname()
  y0 <- exp(exponit0)
  prev <-
    exp(fit.expon$fitted.values)  #previstos destransformados
  r2 <- summary(lm(prev ~ y))$r.squared #ver o coeficiente R2

  return(list(
    r = r,
    y0 = y0,
    time = time,
    prev = unname(prev),
    r2 = r2
  ))

}




