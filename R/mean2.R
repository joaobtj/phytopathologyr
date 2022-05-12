#' mean2
#'
#' @param df A data.frame
#' @param subset First columns to keep
#'
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
#' @export
mean2 <- function(df, subset = 2) {
  df1 <- df[1:subset]
  df2 <- df[(subset + 1):ncol(df)]
  # vector com os tempos
  times <- colnames(df2) %>%
    as.numeric() %>%
    na.omit() %>%
    as.numeric()

  ## calcular a média de duas colunas adjacentes
  a <- list()
  for (i in seq_len(ncol(df2) / 2)) {
    a[[i]] <- df2[, (i * 2 - 1):(i * 2)] %>% rowMeans()
  }

  ## retornar um data.frame
  b <- as_tibble(a, .name_repair = "unique")
  colnames(b) <- times
  return(cbind(df1, b))
}
