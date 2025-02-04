
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MLcompare

**MLcompare** is used for analyzing omics data and classifying disease
sub- types using both traditional machine learning (ML) algorithms and
deep learning models. This shouldserve as a flexible and toolkit for
people who are working on biomarker discovery and classification tasks,
particularly in the context of complex diseases such as cardiovascular
and neurodegenerative disorders.

## Content

- [Installation](#installation)
- [Dependencies](#dependencies)
- [Functions](#functions)
- [Usage](#usage)
  - [Example Workflow](#example-workflow)
- [Future implementations](#Future%20implementations)

------------------------------------------------------------------------

## Installation

If you want to install the package, please clone this repository and
build it with `devtools`.

``` r
install.packages("devtools")
devtools::install_github("danymukesha/LMcompare")
```

## Dependencies

This package depends on the following R packages:

- `caret`: to train traditional machine learning models.
- `h2o`: to build and deploy deep learning models.

In order to install these dependencies, please run the following
commands:

``` r
install.packages(c("caret", "h2o"))
```

To start using `h2o` for deep learning, initialize it with `h2o.init()`.
\> The function `train_deep_learning_model` will shut down `h2o` upon
completion, ensuring resources are managed properly.

## Functions

### `train_models`

I trains a list of traditional ML models on the provided dataset.

- **Parameters:**
  - `data`: data frame with predictor variables and a response variable
    `subtype`.
  - `algorithms`: vector of ML algorithms to train, e.g.,
    `c("rpart", "lda", "svmRadial", "rf", "gbm")`.
- **Returns**: list of trained `caret` models.

### `train_deep_learning_model`

It trains a deep learning model using the `h2o` framework, designed for
binary or multiclass classification of gene expression data (or other
type of omics).

- **Parameters:**
  - `data`: data frame with predictors and response variable `subtype`.
  - `hidden_layers`: vector specifying the number of neurons in each
    hidden layer (e.g., `c(50, 50)`).
  - `epochs`: number of training epochs.
  - `activation`: activation function to use (e.g.,
    `"RectifierWithDropout"`).
- **Returns**: trained `h2o` deep learning model.

## Usage

### Data preparation

The input `data` should contain the following: - “Predictor” variables:
this can be gene expression features or other biomarker data. -
“Response” variable: categorical variable named `subtype`, representing
the target class (e.g., Alzheimer’s Disease subtypes).

> Make sure the data is cleaned and preprocessed before inputting it
> into the models.

### Example workflow

Here is an example of how to use the **MLcompare** package to train and
evaluate both traditional ML and deep learning models on gene expression
data.

#### Load the Package

``` r
library(LMcompare)
```

#### Load and prepare Data

Please load the dataset and ensure it includes predictors and a response
variable `subtype`. Here is an example assuming `data` is already
loaded:

``` r
#you can have a preview of your data, just providing the first rows
head(data)
```

#### Train traditional ML models

The `train_models` function should allow you to train a variety of ML
algorithms, such as decision trees, linear discriminant analysis,
support vector machines, random forests, and gradient boosting machines.

``` r
# here you specify algorithms to train
algorithms <- c("rpart", "lda", "svmRadial", "rf", "gbm")

# you then train the models
ml_models <- train_models(data, algorithms)
```

#### Train a DL model

To train a deep learning model, use `train_deep_learning_model`. This
function utilizes `h2o` to create a deep neural network with
customizable hidden layers, epochs, and activation function.

``` r
h2o::h2o.init()

deep_learning_model <- train_deep_learning_model(
  data,
  hidden_layers = c(100, 50),
  epochs = 20,
  activation = "RectifierWithDropout"
)

# please shut down H2O to free resources
h2o::h2o.shutdown(prompt = FALSE)
```

#### Evaluate model performance

After training, you then should evaluate the model performance using
metrics like AUC, accuracy, and precision.

``` r
for (model_name in names(ml_models)) {
  cat("Model:", model_name, "\n")
  print(ml_models[[model_name]]$results)
}

# for evaluating the deep learning model, use this
h2o::h2o.performance(deep_learning_model, newdata = h2o::as.h2o(data))
```

# Future implementations

There is a plan to continue the development of this package and
implement other useful functions, with the purpose to make it utilizable
for other people working with ML algorithms in research. However, at the
moment I am very busy with other projects, if you feel and will to
contribute to this package, please don’t hesitate to contact
[me](https://danymukesha.github.io/).
