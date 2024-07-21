


load("~/CQRTester/Data/ATData.Rda")

load("Data/ATData.Rda")

source("R/FactorData.R")

datesub <- as.Date("2019-12-31")
datasub <- data[which(data$Periods==datesub),]

test_SPFD <-SinglePeriodFactorData(
  data = datasub,
  date = datesub,
  iname = "Company Id",
  fname = "EPS to Price - Trail",
  rname = "Return_Q01")


test_settings <- ATSettings(testing.scheme = "QSpread")
  


test_settings <- CQRTester::AT_FactorWeighted(c(0.05, 0.95))


test_SPAT <- CQRTester::AlphaTest(test_SPFD, test_settings)

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


