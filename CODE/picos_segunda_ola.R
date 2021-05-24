# Monitoreo del pico en la segunda ola -----
df = readRDS("data_app/historical.rds")
df = df[df$iso == "MD",]
dates = seq.Date(from = as.Date("2020-06-24"), to = as.Date("2020-11-23"), by = "day")
params = readRDS("data_app/params_isos.rds")
params = params[params$iso == "MD",]
source("CODE/functions/models_definition.R")
library(ggplot2)
picos = lapply(dates, 
       function(x) {
         dg = df[df$date <= x,]
         dh = dg[dg$date >= as.Date("2020-06-24"),]
         model = ecm$new(
           data = dg, K = params$K, r = params$r, seasonal = params$seasonal)
         model$train()
         pred = model$predict(h = 100)
         pred$h = NULL
         pred = bind_rows(dg[dg$date >=  as.Date("2020-06-24"), colnames(pred)], pred)  
         data.frame(
           dates = x,
           confirmed = pred$date[which.max(pred$confirmed)],
           hosp = pred$date[which.max(pred$hosp)],
           icu = pred$date[which.max(pred$icu)],
           deaths = pred$date[which.max(pred$deaths)],
           confirmed_real = dh$date[which.max(dh$confirmed)],
           hosp_real = dh$date[which.max(dh$hosp)],
           icu_real = dh$date[which.max(dh$icu)],
           deaths_real = dh$date[which.max(dh$deaths)]
         )
         
  
})
picos = bind_rows(picos)
cols_proj = c("Model" = "#6699CC", "Real" = "black")
picos %>% dplyr::select(dates, confirmed_real, confirmed) %>%
  mutate(day = 1:nrow(picos)) %>%
  mutate(
    n_real = as.numeric(confirmed_real - as.Date("2020-06-24")),
    n_estimado = as.numeric(confirmed - as.Date("2020-06-24"))
  ) %>%
  dplyr::select(dates, confirmed_real, confirmed) %>%
  rename(
    `Model` = confirmed,
    `Real` = confirmed_real
  ) %>%
  tidyr::pivot_longer(-dates, names_to = "Legend") %>%
  ggplot() + 
  geom_line(aes(x = dates, y = value, col = Legend), size = 1) +
  ylab("Date of maximun value") + 
  xlab("Date of mesaure/forecast") + 
  theme_light() +
  ggtitle("Confirmed cases peak",
          subtitle = "Measuring the prediction of the peak of wave")+ 
  theme(
    text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank()
  ) + 
  scale_y_date(limits = c(min(picos$dates), max(picos$dates))) +
  scale_color_manual(values = cols_proj)
