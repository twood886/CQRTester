# Test
library(tidyverse)
library(CQRTester)
library(readr)

load("Data\\data.rda")


# Create Alpha Testing Settings
fcf2ev_settings <- set_at_settings(
  testing_scheme = "factor-z"
)

# Test Single Period AT Data
test_spat_data <- CQRTester:::create_single_period_at_data(
  date = as.Date("2023-01-31"),
  data,
  date_col_name = "periods",
  id_col_name = "company_id",
  factor_col_name = "free_cash_flow_to_enterprise_value",
  return_col_name = "return",
  include_col_name = "include",
  group_col_name = "fact_set_econ_sector",
  horizon = 12
)

fcf2ev_factor_at_sp <- alpha_test(test_spat_data, fcf2ev_settings)



# Test Multi Period AT Data
test_at_data <- create_at_data(
  data,
  date_col_name = "periods",
  id_col_name = "company_id",
  factor_col_name = "free_cash_flow_to_enterprise_value",
  return_col_name = "return",
  include_col_name = "include",
  group_col_name = "fact_set_econ_sector",
  horizon = 12
)

# Alpha Test
fcf2ev_at <- alpha_test(test_at_data, fcf2ev_settings)

# Create Factor Data
fcf2ev_data <- CQRTester::create_factor_data(
  data = data,
  date_col_name = "periods",
  factor_col_name = "free_cash_flow_to_enterprise_value",
  id_col_name = "company_id",
  return_col_name = c(
    "return_m01",
    "return_m03",
    "return_m06",
    "return_m09",
    "return_m12"
  ),
  group_col_name = "fact_set_econ_sector"
)

test <- fcf2ev_data@factor_data[[390]]



