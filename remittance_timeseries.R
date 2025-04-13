library(fpp2)
library(seastests)

# don't forget to set directory with the remittance csv
setwd("D:/New Documents/UPSE_4-2/Econ_132_FIJK/probsets")
OFremit <- read.csv("remittance_ph.csv")

# create objects
OFremit.ts <- ts(OFremit, frequency = 12, start = c(2013,1))
OFremit2.ts <- window(OFremit.ts, start = c(2014,1))

# plots
autoplot(OFremit2.ts[,2])+
  ggtitle("Monthly Nominal Cash Remittances by OFWs, Jan 2014 - Jan 2025")+
  xlab("Year")+
  ylab("Million USD$ ")

ggseasonplot(OFremit2.ts[,2])+
  ggtitle("Seasonal Nominal Cash Remittances by OFWs, Jan 2014 - Jan 2025")+
  xlab("Year")+
  ylab("Million USD$ ")

# test seasonality
help(kw)
kw(OFremit2.ts[,2], freq = 12, diff = TRUE, residuals = FALSE, autoarima = FALSE)

# test & training set
OFremit2.train <- window(OFremit2.ts, end = c(2022,12))
OFremit2.test <- window(OFremit2.ts, start = c(2023,1))

help(AIC)
obs <- nrow(data.frame(OFremit2.train))
# deterministic trend
OFremit2_trend <- tslm(remittance ~ trend, data = OFremit2.train)
summary(OFremit2_trend)
AIC(OFremit2_trend)
AIC(OFremit2_trend, k = log(obs))

# quadratic trend
OFremit2_qtrend <- tslm(remittance ~ poly(trend,2), data = OFremit2.train)
summary(OFremit2_qtrend)
AIC(OFremit2_qtrend)
AIC(OFremit2_qtrend, k = log(obs))

# seasonality
OFremit2_season <- tslm(remittance ~ season, data = OFremit2.train)
summary(OFremit2_season)
AIC(OFremit2_season)
AIC(OFremit2_season, k = log(obs))

# trend+seasonality
OFremit2_trendseason <- tslm(remittance ~ trend + season, data = OFremit2.train)
summary(OFremit2_trendseason)
AIC(OFremit2_trendseason)
AIC(OFremit2_trendseason, k = log(obs))

# quadratic trend+seasonality
OFremit2_qtrendseason <- tslm(remittance ~ poly(trend,2) + season, data = OFremit2.train)
summary(OFremit2_qtrendseason)
AIC(OFremit2_qtrendseason)
AIC(OFremit2_qtrendseason, k = log(obs))

# forecast
print(nrow(data.frame(OFremit2.test))) #25
OFremit2_trendf <- forecast(OFremit2_trend, h=25)
OFremit2_qtrendf <- forecast(OFremit2_qtrend, h=25)
OFremit2_seasonf <- forecast(OFremit2_season, h=25)
OFremit2_trendseasonf <- forecast(OFremit2_trendseason, h=25)
OFremit2_qtrendseasonf <- forecast(OFremit2_qtrendseason, h=25)

# plotting
autoplot(OFremit2.test[,2], series = "Test Series") +
  autolayer(OFremit2_trendf, series = "Deterministic Trend", PI = FALSE) +
  autolayer(OFremit2_qtrendf, series = "Quadratic Trend", PI = FALSE) +
  autolayer(OFremit2_seasonf, series = "Seasonality", PI = FALSE) +
  autolayer(OFremit2_trendseasonf, series = "Trend + Seasonality", PI = FALSE) +
  autolayer(OFremit2_qtrendseasonf, series = "Quadratic Trend + Seasonality", PI = FALSE) +
  xlab("Year") +
  ylab("Million USD$") +
  ggtitle("Forecasts for Monthly Nominal Cash Remittances by OFWs: Jan 2023-Jan 2025") +
  scale_color_manual(values = c("Test Series" = "black",
                                "Deterministic Trend" = "blue",   
                                "Quadratic Trend" = "darkgreen",
                                "Seasonality" = "purple",
                                "Trend + Seasonality" = "orange",
                                "Quadratic Trend + Seasonality" = "red")) +
  guides(color = guide_legend(title = "Forecast"))

# residuals
checkresiduals(OFremit2_trendseasonf)
Box.test(residuals(OFremit2_trendseasonf), lag=4, fitdf=0, type="Ljung-Box")
#H0: white noise/autocorrelation is not significantly different from zero.


# forecast upcoming quarter
OFremit2_trendseasonf_new <- forecast(OFremit2_trendseason, h=27)
OFremit2_trendseasonf_new #2835.905 2767.288 2979.364
OFremit2.test #2835.90 2645.557 2737.834 

Q12025 = 2860.166+2767.288+2979.364
Q12024 = 2835.90+2645.557+2737.834
((Q12025-Q12024)/(Q12024))*100
