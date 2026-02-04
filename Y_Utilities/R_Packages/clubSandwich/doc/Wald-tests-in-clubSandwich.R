## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE---------
library(clubSandwich)
AER_available <- requireNamespace("AER", quietly = TRUE)

knitr::opts_chunk$set(eval = AER_available)

if (!AER_available) cat("# Building this vignette requires the AER package. Please install it. {-}")

## ----message = FALSE, warning = FALSE-----------------------------------------
library(clubSandwich)

data(STAR, package = "AER")

# clean up a few variables
levels(STAR$stark)[3] <- "aide"
levels(STAR$schoolk)[1] <- "urban"
STAR <- subset(STAR, 
               !is.na(schoolidk),
               select = c(schoolidk, schoolk, stark, gender, ethnicity, math1, lunchk))
head(STAR)

## -----------------------------------------------------------------------------
args(Wald_test)

## ----type-treat---------------------------------------------------------------

lm_trt <- lm(math1 ~ stark, data = STAR)
V_trt <- vcovCR(lm_trt, cluster = STAR$schoolidk, type = "CR2")
coef_test(lm_trt, vcov = V_trt)


## -----------------------------------------------------------------------------
C_trt <- matrix(c(0,0,1,0,0,1), 2, 3)
C_trt
Wald_test(lm_trt, constraints = C_trt, vcov = V_trt)

## -----------------------------------------------------------------------------
args(constrain_zero)

## -----------------------------------------------------------------------------
constrain_zero(2:3, coefs = coef(lm_trt))

## -----------------------------------------------------------------------------
constrain_zero(c("starksmall","starkaide"), coefs = coef(lm_trt))

## -----------------------------------------------------------------------------
constrain_zero("^stark", coefs = coef(lm_trt), reg_ex = TRUE)

## -----------------------------------------------------------------------------
C_trt <- constrain_zero(2:3, coefs = coef(lm_trt))
Wald_test(lm_trt, constraints = C_trt, vcov = V_trt)

## -----------------------------------------------------------------------------
Wald_test(lm_trt, constraints = constrain_zero(2:3), vcov = V_trt)

## ----type-sep-----------------------------------------------------------------

lm_sep <- lm(math1 ~ 0 + stark, data = STAR)
V_sep <- vcovCR(lm_sep, cluster = STAR$schoolidk, type = "CR2")
coef_test(lm_sep, vcov = V_sep)


## -----------------------------------------------------------------------------
args(constrain_equal)

## -----------------------------------------------------------------------------
constrain_equal(1:3, coefs = coef(lm_sep))

## -----------------------------------------------------------------------------
constrain_equal(c("starkregular","starksmall","starkaide"), coefs = coef(lm_sep))

## -----------------------------------------------------------------------------
constrain_equal("^stark", coefs = coef(lm_sep), reg_ex = TRUE)

## -----------------------------------------------------------------------------
C_sep <- constrain_equal("^stark", coefs = coef(lm_sep), reg_ex = TRUE)
Wald_test(lm_sep, constraints = C_sep, vcov = V_sep)

## -----------------------------------------------------------------------------
Wald_test(lm_sep, constraints = constrain_equal(1:3), vcov = V_sep)

## -----------------------------------------------------------------------------
lm_urbanicity <- lm(math1 ~ schoolk * stark + gender + ethnicity + lunchk, data = STAR)
V_urbanicity <- vcovCR(lm_urbanicity, cluster = STAR$schoolidk, type = "CR2")
coef_test(lm_urbanicity, vcov = V_urbanicity)

## -----------------------------------------------------------------------------
Wald_test(lm_urbanicity, 
          constraints = constrain_zero("schoolk.+:stark", reg_ex = TRUE),
          vcov = V_urbanicity)

## -----------------------------------------------------------------------------
Wald_test(lm_urbanicity, 
          constraints = constrain_zero("schoolk.+:starksmall", reg_ex = TRUE),
          vcov = V_urbanicity)

## -----------------------------------------------------------------------------
C_list <- list(
  `Any interaction` = constrain_zero("schoolk.+:stark", 
                                     coef(lm_urbanicity), reg_ex = TRUE),
  `Small vs regular` = constrain_zero("schoolk.+:starksmall", 
                                      coef(lm_urbanicity), reg_ex = TRUE)
)

Wald_test(lm_urbanicity, 
          constraints = C_list,
          vcov = V_urbanicity)


## -----------------------------------------------------------------------------

Wald_test(lm_urbanicity, 
          constraints = C_list,
          vcov = V_urbanicity, 
          tidy = TRUE)


## -----------------------------------------------------------------------------

Wald_test(
  lm_urbanicity, 
  constraints = list(
    `Any interaction` = constrain_zero("schoolk.+:stark", reg_ex = TRUE),
    `Small vs regular` = constrain_zero("schoolk.+:starksmall", reg_ex = TRUE)
  ),
  vcov = V_urbanicity, 
  tidy = TRUE
)


## -----------------------------------------------------------------------------
coef_test(lm_sep, vcov = V_sep)

## -----------------------------------------------------------------------------
C_pairs <- constrain_pairwise(1:3, coefs = coef(lm_sep))
C_pairs

## -----------------------------------------------------------------------------
Wald_test(lm_sep, constraints = C_pairs, vcov = V_sep, tidy = TRUE)

## -----------------------------------------------------------------------------
t_stats <- coef_test(lm_trt, vcov = V_trt)$tstat[2:3]
F_stats <- Wald_test(lm_sep, constraints = C_pairs, vcov = V_sep, tidy = TRUE)$Fstat[1:2]
all.equal(t_stats^2, F_stats)

## -----------------------------------------------------------------------------
coef_test(lm_urbanicity, vcov = V_urbanicity)

## -----------------------------------------------------------------------------
Wald_test(lm_urbanicity, 
          constraints = constrain_pairwise(":starksmall", reg_ex = TRUE, with_zero = TRUE),
          vcov = V_urbanicity,
          tidy = TRUE)

