## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  fig.width = 7,
  fig.height = 7,
  fig.align = "center"
)
options(
  tibble.print_min = 4L,
  tibble.print_max = 4L
)

## -----------------------------------------------------------------------------
data("Boston", package = "MASS")
head(Boston)

## ----message = FALSE----------------------------------------------------------
set.seed(42)
library("iml")
library("randomForest")
data("Boston", package = "MASS")
rf <- randomForest(medv ~ ., data = Boston, ntree = 10)

## -----------------------------------------------------------------------------
X <- Boston[which(names(Boston) != "medv")]
predictor <- Predictor$new(rf, data = X, y = Boston$medv)

## -----------------------------------------------------------------------------
imp <- FeatureImp$new(predictor, loss = "mae")
library("ggplot2")
plot(imp)
imp$results

## -----------------------------------------------------------------------------
ale <- FeatureEffect$new(predictor, feature = "lstat", grid.size = 10)
ale$plot()

## -----------------------------------------------------------------------------
ale$set.feature("rm")
ale$plot()

## -----------------------------------------------------------------------------
interact <- Interaction$new(predictor, grid.size = 15)
plot(interact)

## -----------------------------------------------------------------------------
interact <- Interaction$new(predictor, feature = "crim", grid.size = 15)
plot(interact)

## -----------------------------------------------------------------------------
effs <- FeatureEffects$new(predictor, grid.size = 10)
plot(effs)

## -----------------------------------------------------------------------------
tree <- TreeSurrogate$new(predictor, maxdepth = 2)
plot(tree)

## -----------------------------------------------------------------------------
head(tree$predict(Boston))

## -----------------------------------------------------------------------------
lime.explain <- LocalModel$new(predictor, x.interest = X[1, ])
lime.explain$results
plot(lime.explain)

## -----------------------------------------------------------------------------
lime.explain$explain(X[2, ])
plot(lime.explain)

## -----------------------------------------------------------------------------
shapley <- Shapley$new(predictor, x.interest = X[1, ], sample.size = 50)
shapley$plot()

## -----------------------------------------------------------------------------
shapley$explain(x.interest = X[2, ])
shapley$plot()

## -----------------------------------------------------------------------------
results <- shapley$results
head(results)

