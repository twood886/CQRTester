load("Data/ATData.Rda")
source("R/factor_data_single_period.R")
datesub <- as.Date("2019-12-31")
datasub <- data[which(data$Periods == datesub), ]
datasub[1, "EPS to Price - Trail"] <- NA

test_spfd <- create_single_period_factor_data(
  data = datasub,
  date = datesub,
  id_col_name = "Company Id",
  factor_col_name = "EPS to Price - Trail",
  return_col_name = "Return_Q01"
)

source("R/at_settings.R")
test_settings <- set_at_settings(
  testing_scheme = "factor-q",
  weighting_scheme = "equal-short-only",
  quantiles = 3
)

source("R/utilities_scoring.R")
source("R/testing_schemes.R")
source("R/weighting_schemes.R")
source("R/alpha_test_single_period.R")
test_spat <- alpha_test(test_spfd, test_settings)



library(tidyverse)
source("R/generic_methods.R")
source("R/factor_data_params.R")
source("R/factor_data.R")
source("R/Utilities.R")
source("R/factor_data_single_period.R")

test_factor_data <- data %>%
  create_factor_data() %>%
  set_date_col("Periods") %>%
  set_id_col("Company Id") %>%
  set_factor_col("EPS to Price - Trail") %>%
  set_return_col("Return_Q01")
