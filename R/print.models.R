#' @export
print.models <- function(x, ...) {
  print(x$r, x$y0, x$r2, ...)
}
