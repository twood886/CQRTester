# Test
library(CQRTester)

# Load Data
fcf2ev_data <- CQRTester::data

# Create Alpha Testing Settings
fcf2ev_settings <- set_at_settings(
  testing_scheme = "factor-q",
  weighting_scheme = "equal-spread",
  quantile = 5
)

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
?CQRTester::alpha_test

mean(fcf2ev_at@alpha_fwd_return[[4]])
