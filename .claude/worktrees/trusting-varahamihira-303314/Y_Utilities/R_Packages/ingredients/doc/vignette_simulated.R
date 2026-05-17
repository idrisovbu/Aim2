## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
# predict function for the model
the_model_predict <- function(m, x) {
 x$x1 * x$x2 + x$x2
}

# correlated variables 
N <- 50
set.seed(1)
x1 <- runif(N, -5, 5)
x2 <- x1 + runif(N)/100
df <- data.frame(x1, x2)

## -----------------------------------------------------------------------------
library("DALEX")
explain_the_model <- explain(1,
                      data = df,
                      predict_function = the_model_predict)

## -----------------------------------------------------------------------------
library("ingredients")
library("ggplot2")

sample_rows <- data.frame(x1 = -5:5,
                          x2 = -5:5)

cp_model <- ceteris_paribus(explain_the_model, sample_rows)
plot(cp_model) +
  show_observations(cp_model) +
  ggtitle("Ceteris Paribus profiles")

## -----------------------------------------------------------------------------
pd_model <- partial_dependence(explain_the_model, variables = c("x1", "x2"))
pd_model$`_label_` = "PDP"

cd_model <- conditional_dependence(explain_the_model, variables = c("x1", "x2"))
cd_model$`_label_` = "CDP 0.25"

ad_model <- accumulated_dependence(explain_the_model, variables = c("x1", "x2"))
ad_model$`_label_` = "ALE 0.25"

plot(ad_model, cd_model, pd_model) +
  ggtitle("Feature effects - PDP, CDP, ALE")

cd_model_1 <- conditional_dependence(explain_the_model, variables = c("x1", "x2"), span = 0.1)
cd_model_1$`_label_` = "CDP 0.1"

cd_model_5 <- conditional_dependence(explain_the_model, variables = c("x1", "x2"), span = 0.5)
cd_model_5$`_label_` = "CDP 0.5"

ad_model_1 <- accumulated_dependence(explain_the_model, variables = c("x1", "x2"), span = 0.5)
ad_model_1$`_label_` = "ALE 0.1"

ad_model_5 <- accumulated_dependence(explain_the_model, variables = c("x1", "x2"), span = 0.5)
ad_model_5$`_label_` = "ALE 0.5"

plot(ad_model, cd_model, pd_model, cd_model_1, cd_model_5, ad_model_1, ad_model_5) +
  ggtitle("Feature effects - PDP, CDP, ALE")

## -----------------------------------------------------------------------------
# add grouping variable
df$x3 <- factor(sign(df$x2))
# update the data argument
explain_the_model$data = df

# PDP in groups
pd_model_groups <- partial_dependence(explain_the_model, 
                                      variables = c("x1", "x2"), 
                                      groups = "x3")
plot(pd_model_groups) +
  ggtitle("Partial Dependence")

# ALE in groups
ad_model_groups <- accumulated_dependence(explain_the_model, 
                                      variables = c("x1", "x2"), 
                                      groups = "x3")
plot(ad_model_groups) +
  ggtitle("Accumulated Local")


# CDP in groups
cd_model_groups <- conditional_dependence(explain_the_model, 
                                      variables = c("x1", "x2"), 
                                      groups = "x3")
plot(cd_model_groups) +
  ggtitle("Conditional Dependence")

## -----------------------------------------------------------------------------
sessionInfo()

