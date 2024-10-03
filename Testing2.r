# Test
library(tidyverse)
library(CQRTester)
library(readr)

# Read in raw data
data_raw <- read_delim(
  "C:/Users/TWood.callodine/Downloads/Factors_Within_Value - 20240911 - 1418/Constituents - Constituents.txt",
  delim = "\t",
  escape_double = FALSE,
  col_types = cols(Periods = col_date(format = "%m/%d/%Y")),
  trim_ws = TRUE
)


#ctz <- function(x) {
#  win_x <- DescTools::Winsorize(x, na.rm = TRUE)
#  mean_x <- mean(x, na.rm = TRUE)
#  sd_x <- sd(x, na.rm = TRUE)
#  (win_x - mean_x) / sd_x
#}

# Clean data
data <- data_raw %>%
  dplyr::mutate(
    `company_id` = `...1`,
    `return_m01` = `Universe Returns` / 100,
    `return_m03` = `Universe Returns Additional Return 2` / 100,
    `return_m06` = `Universe Returns Additional Return 3` / 100,
    `return_m09` = `Universe Returns Additional Return 4` / 100,
    `return_m12` = `Universe Returns Additional Return 5` / 100,
    .keep = "unused"
  ) %>%
  janitor::clean_names()

'
   %>%
  dplyr::mutate(
    `earnings_yield_z` = ctz(`earnings_yield`, win_prob = c(0.05, 0.95)),
    `ntm_earnings_yield_z` = ctz(`ntm_earnings_yield`,  win_prob = c(0.05, 0.95)),
    `book_to_price_z` = ctz(`book_to_price`,  win_prob = c(0.05, 0.95)),
    `operating_cash_flow_yield_z` = ctz(`operating_cash_flow_yield`, win_prob = c(0.05, 0.95)),
    `free_cash_flow_to_enterprise_value` = ctz(`operating_cash_flow_yield`, win_prob = c(0.05, 0.95)),
    .by = c("periods", "fact_set_econ_sector")
  ) %>%
  dplyr::mutate(
    `value_score` = rowMeans(pick(
      `earnings_yield_z`,
      `ntm_earnings_yield_z`,
      `book_to_price_z`,
      `operating_cash_flow_yield_z`,
      `free_cash_flow_to_enterprise_value`
    ),
    na.rm = TRUE
    )
  )
'



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

# Create Alpha Testing Settings
fcf2ev_settings <- set_at_settings(
  testing_scheme = "factor-z"
)

# Alpha Test
fcf2ev_factor_at_sp <- alpha_test(test, fcf2ev_factor_settings)
fcf2ev_at <- alpha_test(fcf2ev_data, fcf2ev_settings)
