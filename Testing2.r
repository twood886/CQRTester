# Test
library(tidyverse)
library(CQRTester)
library(readr)

# Read in raw data
data_raw <- read_delim(
  "C:/Users/TWood.callodine/Downloads/Factors_Within_Value - 20240911 - 1418/Constituents - Constituents.txt", 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(Periods = col_date(format = "%m/%d/%Y")), 
  trim_ws = TRUE
)

# Clean data
data <- data_raw %>%
  dplyr::rename(
    `company_id` = `...1`,
    `return_m01` = `Universe Returns`,
    `return_m03` = `Universe Returns Additional Return 2`,
    `return_m06` = `Universe Returns Additional Return 3`,
    `return_m09` = `Universe Returns Additional Return 4`,
    `return_m12` = `Universe Returns Additional Return 5`
  ) %>%
  janitor::clean_names()



# Calculate Value Factor AT
value_factor_settings <- CQRTester::set_at_settings(testing_scheme = "factor-z", win_prob = c(0.05,0.95))

earnings_yield_data <- 
  CQRTester::create_factor_data(
    data = data,
    date_col_name = "periods",
    id_col_name = "company_id",
    factor_col_name = "earnings_yield",
    return_col_name = "return_m01",
    group_col_name = "fact_set_econ_sector"
  )

source("R/factor_data.R")
source("R/factor_data_params.R")
test <- set_date_col(earnings_yield_data, "periods")

earnings_yield_at <- CQRTester::alpha_test(earnings_yield_data, factor_settings)
