#' Fit monomolecular model
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
#' @export
#'
#' @examples
#' dap <- seq(10, 60, 10)
#' inc <- c(0.18, 0.56, 0.82, 0.91, 0.96, 0.98)
#' monom(y = inc , time = dap)


monom <- function(y, time) {

  monit <- log(1 / (1 - y)) #transforma a variável
  monom <- lm(monit ~ time)
  r <- monom$coefficients[2] %>% unname()
  monit0 <- monom$coefficients[1] %>% unname()
  y0 = (1 - 1 / exp(monit0))
  prev <-
    (1 - 1 / exp(monom$fitted.values))  #previstos destransformados
  r2 <- summary(lm(prev ~ y))$r.squared #ver o coeficiente R2

  return(list(
    r = r,
    y0 = y0,
    time = time,
    prev = unname(prev),
    r2 = r2
  ))
}
