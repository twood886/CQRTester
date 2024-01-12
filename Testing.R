datesub <- as.Date("2019-12-31")
datasub <- data[which(data$Periods==datesub),]

test_SPFD <- CQRTester::MakeSinglePeriodFactorData(
  data = datasub,
  date = datesub,
  iname = "Company Id",
  fname = "EPS to Price - Trail",
  rname = "Return_Q01")

test_SPAT <- AlphaTest(test_SPFD, 3, c(0.05, 0.95))

