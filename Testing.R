load("C:/Users/TWood.callodine/Callodine Capital Management, LP/TWood - Documents/CQRTester/Data/ATData.Rda")
datesub <- as.Date("2019-12-31")
datasub <- data[which(data$Periods==datesub),]

test_SPFD <-SinglePeriodFactorData(
  data = datasub,
  date = datesub,
  iname = "Company Id",
  fname = "EPS to Price - Trail",
  rname = "Return_Q01")

test_SPAT <- AlphaTest(test_SPFD, 3, c(0.05, 0.95))

test_FD <- FactorData(
  data = data,
  dname = "Periods",
  iname = "Company Id",
  fname = "EPS to Price - Trail",
  rname = "Return_Q01"
)
