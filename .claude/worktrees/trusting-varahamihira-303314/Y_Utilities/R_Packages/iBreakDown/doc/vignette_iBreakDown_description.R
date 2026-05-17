## ----include = FALSE----------------------------------------------------------
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
library("iBreakDown")
library("randomForest")
titanic <- na.omit(titanic)

model_titanic_rf <- randomForest(survived == "yes" ~ .,
                                 data = titanic 
                                 )

explain_titanic_rf <- explain(model_titanic_rf,
                            data = titanic[,-9],
                            y = titanic$survived == "yes",
                            label = "Random Forest")

passanger <- titanic[sample(nrow(titanic), 1) ,-9]
passanger

## -----------------------------------------------------------------------------
bd_rf <- break_down(explain_titanic_rf,
                    passanger,
                    keep_distributions = TRUE) # distributions should be kept
shap_rf <- shap(explain_titanic_rf,
                passanger)

plot(bd_rf)
plot(shap_rf)

## -----------------------------------------------------------------------------
describe(bd_rf)
describe(shap_rf)

## -----------------------------------------------------------------------------
describe(bd_rf, nonsignificance_treshold = 1)

## -----------------------------------------------------------------------------
describe(bd_rf, 
         label = "the passanger survived with probability")

## -----------------------------------------------------------------------------
describe(bd_rf, short_description = TRUE)

## -----------------------------------------------------------------------------
describe(bd_rf, display_values = TRUE)

## -----------------------------------------------------------------------------
describe(bd_rf, display_numbers = TRUE)

## -----------------------------------------------------------------------------
describe(bd_rf, display_distribution_details = TRUE)

## -----------------------------------------------------------------------------
describe(shap_rf, display_shap = TRUE)

## -----------------------------------------------------------------------------
describe(shap_rf,
         label = "the passanger survived with probability",
         display_values = TRUE,
         display_numbers = TRUE,
         display_shap = TRUE)

