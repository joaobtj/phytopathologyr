#' Fit Logistic model
#'
#' @param y Proportion of the disease. Must be a value between 0 and 1.
#' @param time Measurement time
#'
#' @return r Growth rate
#' @return y0 Initial inoculum
#' @return time Measurement time
#' @return pred Predicted data
#' @return r2 Determination coefficient between the observed and predicted data
#'
#' @importFrom magrittr %>%
#' @importFrom stats lm
#'
#' @examples
#' dap <- seq(1, 70, 10)
#' inc <- c(0.01, 0.04, 0.15, 0.31, 0.65, 0.88, 0.94)
#' logist(y = inc, time = dap)
#' @export
logist <- function(y, time) {
  logit <- log(y / (1 - y)) # transforma a variável
  fit.logist <- lm(logit ~ time)
  r <- fit.logist$coefficients[2] %>% unname()
  logit0 <- fit.logist$coefficients[1]
  y0 <- (exp(logit0) / (1 + exp(logit0))) %>% unname()
  pred <-
    exp(fit.logist$fitted.values) / (1 + exp(fit.logist$fitted.values)) # previstos destransformados
  r2 <- summary(lm(pred ~ y))$r.squared # ver o coeficiente R2

  r <- (list(
    r = r,
    y0 = y0,
    time = time,
    pred = unname(pred),
    r2 = r2
  ))

  class(r) <- "model"


  return(r)
}
