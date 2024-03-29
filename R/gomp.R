#' Fit Gompertz model
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
#' inc <- c(0.01, 0.14, 0.40, 0.65, 0.88, 0.96, 0.98)
#' gomp(y = inc, time = dap)
#' @export
gomp <- function(y, time) {
  gompit <- -log(-log(y)) # transforma a variável
  fit.gomp <- lm(gompit ~ time)
  r <- fit.gomp$coefficients[2] %>% unname()
  gompit0 <- fit.gomp$coefficients[1] %>% unname()
  y0 <- exp(-exp(-gompit0))
  pred <-
    exp(-exp(-fit.gomp$fitted.values)) # previstos destransformados
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
