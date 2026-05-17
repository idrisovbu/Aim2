## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", fig.width = 7, fig.height = 7, fig.align = "center")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## -----------------------------------------------------------------------------
set.seed(42)
library("iml")
library("randomForest")
data("Boston", package = "MASS")
rf <- randomForest(medv ~ ., data = Boston, n.trees = 10)
X <- Boston[which(names(Boston) != "medv")]
predictor <- Predictor$new(rf, data = X, y = Boston$medv)

## -----------------------------------------------------------------------------
library("future")
library("future.callr")
# Creates a PSOCK cluster with 2 cores
plan("callr", workers = 2)

## -----------------------------------------------------------------------------
imp <- FeatureImp$new(predictor, loss = "mae")
library("ggplot2")
plot(imp)

## -----------------------------------------------------------------------------
bench::system_time({
  plan(sequential)
  FeatureImp$new(predictor, loss = "mae")
})
bench::system_time({
  plan("callr", workers = 2)
  FeatureImp$new(predictor, loss = "mae")
})

## -----------------------------------------------------------------------------
bench::system_time({
  plan(sequential)
  FeatureImp$new(predictor, loss = "mae", n.repetitions = 10)
})

bench::system_time({
  plan("callr", workers = 2)
  FeatureImp$new(predictor, loss = "mae", n.repetitions = 10)
})

## -----------------------------------------------------------------------------
bench::system_time({
  plan(sequential)
  Interaction$new(predictor, grid.size = 15)
})
bench::system_time({
  plan("callr", workers = 2)
  Interaction$new(predictor, grid.size = 15)
})

## -----------------------------------------------------------------------------
bench::system_time({
  plan(sequential)
  FeatureEffects$new(predictor)
})
bench::system_time({
  plan("callr", workers = 2)
  FeatureEffects$new(predictor)
})

