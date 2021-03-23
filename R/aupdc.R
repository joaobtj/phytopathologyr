#' AUDPC
#'
#' @param y Ammount of the disease.
#' @param time Measurement time
#'
#' @return area Area under the disease progress curve
#'
#' @examples
#' day <- seq(10,60,10)
#' sev <- c(0.22, 0.26, 0.28, 0.36, 0.39, 0.42)
#' audpc(y=sev, time=day)

#' @export
audpc <- function(y, time) {
  area <- 0
  for (i in 1:(length(y) - 1)) {
    area = area + (y[i + 1] + y[i]) / 2 * (time[i + 1] - time[i])
  }

  return(area)

}

