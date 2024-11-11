#' Plot AUC
#'
#' @param results Data frame containing the model names and AUC values.
#' @return A ggplot object with AUC plots for each model.
#' @export
plot_auc <- function(results) {
  ggplot(results, aes(x = Model, y = AUC, fill = Model)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "AUC for Different Models", y = "AUC") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
