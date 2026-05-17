## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  fig.width = 7,
  fig.height = 3.5,
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library("DALEX")
head(titanic)

## -----------------------------------------------------------------------------
# prepare model
library("randomForest")
titanic <- na.omit(titanic)
model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
                                   fare + sibsp + parch,  data = titanic)
model_titanic_rf

## -----------------------------------------------------------------------------
library("DALEX")
explain_titanic_rf <- explain(model_titanic_rf, 
                      data = titanic[,-9],
                      y = titanic$survived == "yes", 
                      label = "Random Forest v7")

## -----------------------------------------------------------------------------
new_passanger <- data.frame(
  class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew", "engineering crew", "restaurant staff", "victualling crew")),
  gender = factor("male", levels = c("female", "male")),
  age = 8,
  sibsp = 0,
  parch = 0,
  fare = 72,
  embarked = factor("Southampton", levels = c("Belfast", "Cherbourg", "Queenstown", "Southampton"))
)

## -----------------------------------------------------------------------------
library("iBreakDown")
rf_la <- local_attributions(explain_titanic_rf, new_passanger)
rf_la

## -----------------------------------------------------------------------------
plot(rf_la)

## -----------------------------------------------------------------------------
plotD3(rf_la)

## -----------------------------------------------------------------------------
rf_la_un <- break_down_uncertainty(explain_titanic_rf, new_passanger,
                         path = "average")
plot(rf_la_un)

## -----------------------------------------------------------------------------
plotD3(rf_la, max_features = 3)

## -----------------------------------------------------------------------------
plotD3(rf_la, max_features = 3, min_max = c(0,1), margin = 0)

