#' Fit models
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
#' @importFrom purrr map
#' @importFrom dplyr summarise
#'
#' @examples
#' time=dap <- seq(10, 60, 10)
#' y=inc <- c(0.18, 0.56, 0.82, 0.91, 0.96, 0.98)
#' fit <- fit_models(y=inc, time=dap)
#' fit

#' @export
fit_models <- function(y, time) {
  f <- data.frame(y, time) %>% summarise(
    linear = lin(y, time),
    exponential = expon(y, time),
    monomolecular = monom(y, time),
    logistic = logist(y, time),
    gompertz = gomp(y, time)
  )

  #order of models by highest r2
  ord <- map(f, "r2") %>% unlist() %>% order(decreasing = TRUE)

  r <- list(fitted = data.frame(
    r = map(f[ord], "r") %>% unlist(),
    y0 = map(f[ord], "y0") %>% unlist(),
    r2 = map(f[ord], "r2") %>% unlist()
  ))

  class(r) <- "fit_models"

  return(r)

}

