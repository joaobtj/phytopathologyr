#' Fit Gompertz model
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
#' dap <- seq(1, 70, 10)
#' inc <- c(0.01, 0.14, 0.40, 0.65, 0.88, 0.96, 0.98)
#' gomp(y=inc, time=dap)


gomp <- function(y, time) {

  gompit <- -log(-log(y)) #transforma a variável
  gomp <- lm(gompit ~ time)
  r <- gomp$coefficients[2] %>% unname()
  gompit0 <- gomp$coefficients[1] %>% unname()
  y0 <- exp(-exp(-gompit0))
  prev <-
    exp(-exp(-gomp$fitted.values))  #previstos destransformados
  r2 <- summary(lm(prev ~ y))$r.squared #ver o coeficiente R2

  return(list(
    r = r,
    y0 = y0,
    time = time,
    prev = unname(prev),
    r2 = r2
  ))

}
