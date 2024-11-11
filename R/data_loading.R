#' Load Dataset
#'
#' @param file_path The file path to the dataset.
#' @return A data frame containing the loaded dataset.
#' @export
load_data <- function(file_path) {
  data <- read.csv(file = file_path, check.names = TRUE)
  data <- data[, -1]  # Remove sample labels
  data$subtype <- as.factor(ifelse(data$subtype == 1, 1, 0))
  return(data)
}
