load("Data/ATData.Rda")
source("R/SinglePeriodFactorData.R")
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

source("R/ATSettings.R")
test_settings <- set_at_settings(weighting_scheme = "long-only", win_prob = c(.05,.95))
test_settings <- set_at_settings(
  testing_scheme = "factor-q",
  weighting_scheme = "equal-short-only",
  quantiles = 3
)

source("R/Utilities_Scoring.R")
source("R/testing_schemes.R")
source("R/weighting_schemes.R")
source("R/SinglePeriodAT.R")
test_spat <- alpha_test(test_spfd, test_settings)


source("R/FactorData.R")
source("R/Utilities.R")

test_params <- create_factor_data_params()
check_factor_data_params(test_params)


test_fd <- create_factor_data(data)




test_FD <- FactorData(
  data = data,
  dname = "Periods",
  iname = "Company Id",
  fname = "EPS to Price - Trail",
  rname = "Return_Q01"
)


subdate <- as.Date("2019-12-31")
subdata <- data[which(data$Periods==subdate),]

test_SPFD <- MakeSinglePeriodFactorData(
  subdata,
  subdate,
  "Company Id",
  "EPS to Price - Trail",
  "Return_Q01")

test_SPAT <- AlphaTest2(test_SPFD)



test_settings <- ATSettings()


test_FD <- FactorData(
  data,
  dname = "Periods",
  iname = "Company Id",
  fname = "EPS to Price - Trail",
  rname = "Return_Q01")




# Show Methods ------------------------------------------------------------


