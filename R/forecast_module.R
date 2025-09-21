# forecast_module.R
library(forecast)

forecast_arima <- function(ts_obj, h = 7) {
  fit <- auto.arima(ts_obj)
  fc <- forecast(fit, h = h)
  list(fit = fit, forecast = fc)
}
