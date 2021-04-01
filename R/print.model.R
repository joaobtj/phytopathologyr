#' @export
print.model <- function(x, ...) {
  cat("adjusted parameters \n")
  cd <- data.frame(r=x$r, y0=x$y0, r2=x$r2)
  print(cd, row.names=FALSE)
  invisible(x)
}
