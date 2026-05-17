## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  fig.width = 7,
  fig.height = 3.5,
  warning = FALSE,
  message = FALSE
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library("DALEX")
library("ingredients")
library("ranger")

model_titanic_rf <- ranger(survived ~ ., data = titanic_imputed, probability = TRUE)

explain_titanic_rf <- explain(model_titanic_rf,
                            data = titanic_imputed[,-8],
                            y = titanic_imputed[,8],
                            label = "Random Forest")

passanger <- titanic_imputed[sample(nrow(titanic_imputed), 1) ,-8]
passanger

## -----------------------------------------------------------------------------
importance_rf <- feature_importance(explain_titanic_rf)
plot(importance_rf)

## -----------------------------------------------------------------------------
describe(importance_rf)

## -----------------------------------------------------------------------------
perturbed_variable <- "class"
cp_rf <- ceteris_paribus(explain_titanic_rf,
                         passanger,
                         variables = perturbed_variable)
plot(cp_rf, variable_type = "categorical")

## -----------------------------------------------------------------------------
describe(cp_rf)

## -----------------------------------------------------------------------------
describe(cp_rf,
         display_numbers = TRUE,
         label = "the probability that the passanger will survive")

## -----------------------------------------------------------------------------
describe(cp_rf,
         display_numbers = TRUE,
         label = "the probability that the passanger will survive",
         variables = perturbed_variable)

## -----------------------------------------------------------------------------
perturbed_variable_continuous <- "age"
cp_rf <- ceteris_paribus(explain_titanic_rf,
                         passanger)
plot(cp_rf, variables = perturbed_variable_continuous)
describe(cp_rf, variables = perturbed_variable_continuous)

## -----------------------------------------------------------------------------
pdp <- aggregate_profiles(cp_rf, type = "partial")
plot(pdp, variables = "fare")
describe(pdp, variables = "fare")

## -----------------------------------------------------------------------------
pdp <- aggregate_profiles(cp_rf, type = "partial", variable_type = "categorical")
plot(pdp, variables = perturbed_variable)
describe(pdp, variables = perturbed_variable)

