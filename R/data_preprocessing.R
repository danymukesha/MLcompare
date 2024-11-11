#' Preprocess Data
#'
#' @param data Data frame containing the metabolite data and subtype labels.
#' @return A data frame with quantile-normalized metabolite data.
#' @export
preprocess_data <- function(data) {
  data_norm <- normalize.quantiles(t(as.matrix(data[, -ncol(data)])))
  data[1:(ncol(data) - 1)] <- t(data_norm)
  return(data)
}
