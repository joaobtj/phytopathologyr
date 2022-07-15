#' MVI
#'
#' @param y Ammount of the disease.
#' @param time Measurement time
#'
#' @return mvi Mean velocity index
#'
#' @examples
#' day <- seq(10, 60, 10)
#' sev <- c(0.22, 0.26, 0.28, 0.36, 0.39, 0.42)
#' plot(sev~day)
#' mvi(y = sev, time = day)
#' @export
mvi <- function(y, time) {
  mvi <- 0
  for (i in 1:(length(y) - 1)) {
    mvi <- mvi + (y[i + 1] - y[i]) /  (time[i + 1] - time[i])
  }

  return(mvi/(length(y) - 1))
}
