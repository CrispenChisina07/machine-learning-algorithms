library(tseries)
library(quantmod)
library(lubridate)
library(timeSeries)
library(forecast)
library(xts)

spy = getSymbols(Symbols = "SPY", auto.assign = FALSE, from ="2020-01-01", to="2021-07-31")

spy_close_prices = "SPY"%>%get_stock_prices()
spy_close_prices = spy_close_prices[,c('Date', 'Close')]
colnames(spy_close_prices) = c('ds', 'y')
m <- prophet(spy_close_prices, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

plot(spy_close_prices)

par(mfrow=c(1,2))

Acf(spy_close_prices, main = "Acf for diffeentiated prices")
Pacf(spy_close_prices, main = "Pacf for diffeentiated prices")

print(adf.test(spy_close_prices))
auto.arima(spy_close_prices, seasonal = F)

fitA = auto.arima(spy_close_prices, seasonal = F)
tsdisplay(residuals(fitA), lag.max = 30, main = "Residuals")
auto.arima(spy_close_prices, seasonal = F)

fitB = arima(spy_close_prices, order = c(1,2,4))
tsdisplay(residuals(fitB), lag.max = 30, main = "Residuals")


term = 560
fcast1 = forecast(fitA, h=term)
plot(fcast1)

fcast2 = forecast(fitB, h=term)
plot(fcast2)

accuracy(fcast1)
accuracy(fcast2)



