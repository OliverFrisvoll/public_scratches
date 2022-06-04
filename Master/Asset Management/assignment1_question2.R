# Build for R 4.1.3

# Removes residual variables from the environment.
rm(list = ls())

# Importing libraries
library(tidyverse)
library(readxl)

# Setting working directory to the location of this file.
rstudioapi::getSourceEditorContext()$path |>
  dirname() |>
  setwd()

# Importing and cleaning the data
HF_index <- "HF_index.xlsx" %>%
  read_excel(skip = 1) %>%
  select(where(~sum(!is.na(.)) > 0))
# Names the columns
colnames(HF_index) <- HF_index %>%
  colnames() %>%
  gsub(" ", "_", .) %>%
  gsub("/", "_", .) %>%
  gsub("-", "_", .)


#' Model generator
#'
#' Generates a list of lm models.
#'
#' @param df - data.frame with the variables needed for the regressions
#' @param forms - character describing the regressands of the model
#' @param indicies - character vector, the indicies to use as regressors
#'
#' @return list of lm objects names based on the regressor used.
#'
model_generator <- function(df, form, indicies) {
  HF <- indicies %>%
    paste0(form) %>%
    map(~lm(formula = ., data = df))

  # Renames
  names(HF) <- indicies

  return(HF)
}


#' Creating a data.frame with coefficients from a list of lm objects.
#'
#' Takes a list of lm objects, extracts the coefficients with corresponding
#' t-stats and combines everyting into one data.frame object
#'
#' @params model - takes a list of lm objects
#'
#' @return data.frame object with coefficients and corresponding t-stat from the model
#'
summarise_coeffs <- function(model) {

  t_stat <- model %>%
    map(~summary(.)$coefficients) %>%
    map_dfr(~.[, 3]) %>%
    rename(
      Alpha = `(Intercept)`
    ) %>%
    mutate(
      across(everything(), ~round(., 4)),
      HF_index = names(model),
      Type = "t_stat"
    )
  Coeff <- model %>%
    map(~summary(.)$coefficients) %>%
    map_dfr(~.[, 1]) %>%
    rename(
      Alpha = `(Intercept)`
    ) %>%
    mutate(
      across(everything(), ~round(., 4)),
      # Annualising the alpha and turning them into percent.
      Alpha = (Alpha * 12 * 100) %>% str_c("%"),
      HF_index = names(model),
      Type = "Coefficients"
    ) %>%
    rbind(t_stat) %>%
    relocate(HF_index, .before = Alpha) %>%
    arrange(HF_index)
}

# Calculates the excess return for all indicies, assumes long/short strategies.
HF_index_excess <- HF_index %>%
  mutate(
    mutate(across(Ln_Sh_Eq_Hedge_Fund_USD:DJCS_Hedge_Fund_USD, ~. - RF))
  )

# Question 2
# (a) (Factor models

# Indicies to regress.
indicies_ff <- c("Ln_Sh_Eq_Hedge_Fund_USD", "Cnvrt_Arb_Hedge_Fund_USD")

model_ff <- NULL
# Univariate model for both indicies
model_ff$HF_MktRF <- HF_index_excess %>%
  model_generator(" ~ Mkt_RF", indicies_ff) %>%
  summarise_coeffs()


# Multivariate model for both indicies
model_ff$HF_MktRF_SMB_HML_UMD <- HF_index_excess %>%
  model_generator(" ~ Mkt_RF + SMB + HML + UMD", indicies_ff) %>%
  summarise_coeffs()


# (b) (Stale Prices)
indicies_sp <- HF_index %>%
  select(Ln_Sh_Eq_Hedge_Fund_USD:DJCS_Hedge_Fund_USD) %>%
  colnames()


model_sp <- NULL
# Univariate model for all indicies (Benchmark)
model_sp$HF_MktRF <- HF_index_excess %>%
  model_generator(" ~ Mkt_RF", indicies_sp) %>%
  summarise_coeffs()


# Regression of HF indicies on MKt_Rf, and 1, 2 month lagged
model_sp$HF_MktRF_L1_L2 <- HF_index_excess %>%
  mutate(
    Mkt_RF_L1 = lag(Mkt_RF, n = 1L),
    Mkt_RF_L2 = lag(Mkt_RF, n = 2L),
  ) %>%
  model_generator(" ~ Mkt_RF + Mkt_RF_L1 + Mkt_RF_L2", indicies_sp) %>%
  summarise_coeffs() %>%
  mutate(
    sum_beta = if_else(Type == "Coefficients", Mkt_RF + Mkt_RF_L1 + Mkt_RF_L2, 0)
  )
