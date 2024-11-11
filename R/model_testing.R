#' Test Models
#'
#' @param models A list of trained models.
#' @param test_data Testing data frame.
#' @return A data frame with performance metrics for each model.
#' @export
test_models <- function(models, test_data) {
  results <- data.frame()
  for (name in names(models)) {
    model <- models[[name]]
    pred <- stats::predict(model, newdata = test_data, type = "prob")
    roc <- pROC::roc(
      predictor = pred$X1,
      response = test_data$subtype,
      levels = rev(levels(test_data$subtype))
    )

    confusion <- caret::confusionMatrix(predict(model, newdata = test_data),
                                 test_data$subtype)
    metrics <- c(
      AUC = roc$auc,
      Sensitivity = confusion$byClass["Sensitivity"],
      Specificity = confusion$byClass["Specificity"],
      Accuracy = confusion$overall["Accuracy"],
      Precision = confusion$byClass["Pos Pred Value"],
      F1 = confusion$byClass["F1"],
      Balanced_Accuracy = confusion$byClass["Balanced Accuracy"]
    )
    results <- rbind(results, data.frame(Model = name, metrics))
  }
  return(results)
}
