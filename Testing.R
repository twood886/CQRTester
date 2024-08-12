library(tidyverse)
load("Data/at_test_data.Rda")
source("R/factor_data_single_period.R")

datesub <- as.Date("1996-06-30")
datasub <- data[which(data$Periods == datesub), ]
datasub[1, "EPS to Price - Trail"] <- NA

test_spfd <- create_single_period_factor_data(
  data = datasub,
  date_col_name = "Periods",
  id_col_name = "Company Id",
  factor_col_name = "EPS to Price - Trail",
  return_col_name = "Return_Q01"
)

source("R/generic_methods.R")
source("R/at_settings.R")
test_settings <- set_at_settings(
  testing_scheme = "factor-q"
)

source("R/factor_z_score.R")
source("R/factor_q_score.R")
source("R/calc_weights.R")
source("R/alpha_test_single_period.R")
source("R/calc_bench_weights.R")
test_spat <- alpha_test(test_spfd, test_settings)


source("R/factor_data_params.R")
source("R/factor_data.R")
source("R/utilities.R")
source("R/factor_data_single_period.R")

test_factor_data <- data %>%
  create_factor_data() %>%
  set_date_col("Periods") %>%
  set_id_col("Company Id") %>%
  set_factor_col("EPS to Price - Trail") %>%
  set_return_col("Return_Q01")


test_settings <- set_at_settings(
  testing_scheme = "factor-q"
)


source("R/alpha_test.R")
test_at <- alpha_test(test_factor_data, test_settings)
