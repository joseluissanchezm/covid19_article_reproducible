df = readRDS("data_app/historical.rds")
df = df[df$iso == "MD",]
params = readRDS("data_app/params_isos.rds")
params = params[params$iso == "MD",]
source("CODE/functions/models_definition.R")
dg = df[df$date <= as.Date("2021-04-01"),]

model = ecm$new(
  data = dg, K = params$K, r = params$r, seasonal = params$seasonal)
model$train()

# ACF -----

library(forecast)
library(tseries)
library(ggplot2)
adf.test(model$residuals$confirmed)
adf.test(model$residuals$hosp)
adf.test(model$residuals$icu)
adf.test(model$residuals$deaths)
pp.test(model$residuals$confirmed)
pp.test(model$residuals$hosp)
pp.test(model$residuals$icu)
pp.test(model$residuals$deaths)

ggAcf(model$residuals$confirmed, lag.max = 50, size = 1.1) + 
  ggtitle("Confirmed",
          subtitle = "ADF test pvalue: <0.01, Phillips - Perron test pvalue: <0.01") + 
  ylab("Autocorrelation") + 
  theme_light() + 
  ylim(-0.5, 0.5) + 
  theme(text = element_text(face = "bold"))

