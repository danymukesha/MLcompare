#' Train Basic ML Models
#'
#' @param data Training data frame.
#' @param algorithms A vector of algorithms to train,
#'    such as c('rpart', 'lda', 'svmRadial', 'rf', 'gbm').
#' @return A list of trained models.
#' @export
train_models <- function(data,
                         algorithms = c('rpart', 'lda', 'svmRadial', 'rf',
                                        'gbm')) {
  models <- list()
  control <- caret::trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  response <- 'subtype'
  predictors <- setdiff(names(data), response)

  for (algo in algorithms) {
    set.seed(7)
    model <- caret::train(
      as.formula(paste(response, "~ .")),
      data = data,
      method = algo,
      trControl = control,
      metric = "ROC"
    )
    models[[algo]] <- model
  }
  return(models)
}

#' Train Deep Learning Model using H2O
#'
#' @param data Training data frame with predictors and response variable.
#' @param hidden_layers A vector specifying the number of neurons
#'    in each hidden layer (e.g., c(50, 50)).
#' @param epochs Number of epochs for training.
#' @param activation Activation function to use, e.g., 'RectifierWithDropout'.
#' @return A trained H2O deep learning model.
#' @export
train_deep_learning_model <- function(data,
                                      hidden_layers = c(50, 50),
                                      epochs = 10,
                                      activation = "RectifierWithDropout") {

  assign("is_h2o_running", T, .GlobalEnv)

  tryCatch(
    expr = {
      h2o.init(startH2O = FALSE)
    },
    error = function(e){
      print(e)
      assign("is_h2o_running", F, .GlobalEnv)
    },
    warning = function(w){
      print(w)
    }
  )
  print(paste0("Is H2O running : ", is_h2o_running))
  # init H2O if not already running
  if (!is_h2o_running) {
    h2o::h2o.init(nthreads = -1)
    on.exit(h2o::h2o.shutdown(prompt = FALSE))
  }

  # convert data to H2O frame
  h2o_data <- h2o::as.h2o(data)

  # please define the response and predictors
  response <- "subtype" # can be changed depending on ur data
  predictors <- setdiff(names(data), response)

  model <- h2o::h2o.deeplearning(
    x = predictors,
    y = response,
    training_frame = h2o_data,
    activation = activation,
    hidden = hidden_layers,
    epochs = epochs
  )

  return(model)
}

