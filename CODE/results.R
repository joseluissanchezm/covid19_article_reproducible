library(dplyr)
library(tidyr)
library(latex2exp)
library(ggplot2)
library(lubridate)
source("CODE/functions/models_definition.R")
source("CODE/functions/forecast_plot.R", encoding = "UTF8")
load("article_results/list_output_datesokFinal.rdata")
# BEATRIZs MODEL ------
results_beatriz = list.dirs("article_results/ArticuloResultados", recursive = TRUE)  %>%
  tail(-1) %>%
  paste(., "/modelo_futuro.csv", sep = "") %>%
  lapply(function(x) {
    df = read.csv(x, sep = ";")
    df$type = if_else(is.na(df$R2ajuste), "prediccion", "ajuste")
    df = df[c("n", "modelodelta", "type")] 
    df$value = df$modelodelta*df$n
    df$date = seq.Date(from = as.Date("2020-06-24"), by = "day", length.out = nrow(df))
    df[c("date", "value", "type")] %>% 
      head(-6)
  })


results_beatriz = results_beatriz %>%
  bind_rows(.id = "test_set") 


test_sets = results_beatriz %>%
  filter(type == "ajuste") %>%
  group_by(test_set) %>%
  summarise(
    last_date_train = max(date)
  )

results_beatriz = results_beatriz %>%
  left_join(test_sets)

results_beatriz$model = "Non linear regression model (MATGEN)"
results_beatriz$test_set = NULL

# SIR Model -------
resultados_sir = names(loutFinal) %>%
  lapply(function(x) {
    loutFinal[[x]] %>% 
      mutate(last_date_train = x) %>%
      filter(date <= (as.Date(last_date_train) + days(15))) %>%
      mutate(
        type = ifelse(date <= as.Date(x), "ajuste", "prediccion")
      ) %>%
      dplyr::select(date, out_confirmed, type, last_date_train) %>%
      rename(value = out_confirmed)
  }) %>% bind_rows()


resultados_sir$model = "SIR"
resultados_sir$date = as.Date(resultados_sir$date)
resultados_sir$last_date_train = as.Date(resultados_sir$last_date_train)

# JOSES MODEL ------

iso = "MD"
last_date_use = as.Date("2021-04-01")
df = readRDS("data_app/historical.rds")
params = readRDS("data_app/params_isos.rds")
df = df[df$iso == iso,]
params = params[params$iso == iso,]



dates_test = test_sets$last_date_train

results_jose = lapply(dates_test,
                      function(date) {
                        dg = df[df$date <= date,]
                        
                        # Initialize model ------
                        model = ecm$new(
                          data = dg, K = params$K, r = params$r, seasonal = params$seasonal)
                        
                        # Train model -----
                        model$train()
                        
                        # Fitted series
                        fitted = model$fitted[c("date", "confirmed")]
                        fitted = filter(fitted, date >= as.Date("2020-06-24"))
                        fitted$last_date_train = date
                        fitted$type = "ajuste"
                        fitted = rename(fitted, value = confirmed)
                        # Puntual predict
                        punt_pred = model$predict(h = 15)
                        punt_pred$last_date_train = date
                        punt_pred$type = "prediccion"
                        punt_pred = punt_pred[c("date", "confirmed", "last_date_train", "type")]
                        punt_pred = rename(punt_pred, value = confirmed)
                        
                        return(bind_rows(fitted, punt_pred))
                      })
results_jose = bind_rows(results_jose)
results_jose$model = "Error correction model (MATGEN)"

# AUTOARIMA MODEL ------
library(forecast)
iso = "MD"
last_date_use = as.Date("2021-04-01")
df = readRDS("data_app/historical.rds")
params = readRDS("data_app/params_isos.rds")
df = df[df$iso == iso,]
params = params[params$iso == iso,]


auto_model = NULL
dates_test = test_sets$last_date_train

auto_model = auto.arima(ts(df[df$date <= dates_test[1],]$confirmed, frequency = 7), lambda = "auto")

results_auto = lapply(dates_test,
                      function(date) {
                        dg = df[df$date <= date,]
                        
                        # Initialize model ------
                        auto_model = Arima(ts(dg$confirmed, frequency = 7),
                                           model = auto_model)
                        
                        
                        
                        # Fitted series
                        fitted = data.frame(
                          date = dg$date, 
                          confirmed = auto_model$fitted %>% as.numeric())
                        fitted = filter(fitted, date >= as.Date("2020-06-24"))
                        fitted$last_date_train = date
                        fitted$type = "ajuste"
                        fitted = rename(fitted, value = confirmed)
                        # Puntual predict
                        punt_pred = data.frame(
                          date = tail(seq.Date(from = date, by = "day", length.out = 16),-1),
                          confirmed = forecast(auto_model, 15)$mean %>% as.numeric())
                        punt_pred$last_date_train = date
                        punt_pred$type = "prediccion"
                        punt_pred = punt_pred[c("date", "confirmed", "last_date_train", "type")]
                        punt_pred = rename(punt_pred, value = confirmed)
                        
                        return(bind_rows(fitted, punt_pred))
                      })
results_auto = bind_rows(results_auto)
results_auto$model = "Auto Arima"

# NN MODEL ------
library(forecast)
iso = "MD"
last_date_use = as.Date("2021-04-01")
df = readRDS("data_app/historical.rds")
params = readRDS("data_app/params_isos.rds")
df = df[df$iso == iso,]
params = params[params$iso == iso,]


dates_test = test_sets$last_date_train

nn_model = nnetar(ts(df[df$date <= dates_test[1],]$confirmed, frequency = 7),
                  P = 1, size = 50, lambda = "auto", scale.inputs = TRUE)

results_nn = lapply(dates_test,
                    function(date) {
                      dg = df[df$date <= date,]
                      
                      # Initialize model ------
                      nn_model = nnetar(ts(dg$confirmed, frequency = 7),
                                        model = nn_model)
                      
                      
                      
                      # Fitted series
                      fitted = data.frame(
                        date = dg$date, 
                        confirmed = nn_model$fitted %>% as.numeric())
                      fitted = filter(fitted, date >= as.Date("2020-06-24"))
                      fitted$last_date_train = date
                      fitted$type = "ajuste"
                      fitted = rename(fitted, value = confirmed)
                      # Puntual predict
                      punt_pred = data.frame(
                        date = tail(seq.Date(from = date, by = "day", length.out = 16),-1),
                        confirmed = forecast(nn_model, 15)$mean %>% as.numeric())
                      punt_pred$last_date_train = date
                      punt_pred$type = "prediccion"
                      punt_pred = punt_pred[c("date", "confirmed", "last_date_train", "type")]
                      punt_pred = rename(punt_pred, value = confirmed)
                      
                      return(bind_rows(fitted, punt_pred))
                    })
results_nn = bind_rows(results_nn)
results_nn$model = "Neural Network"

# PREDICCIONES ------
results = bind_rows(results_beatriz, results_jose, resultados_sir, results_auto, results_nn)
results = filter(results, last_date_train != as.Date("2020-08-01"))
real = df %>% dplyr::select(date, confirmed) %>% rename(real = confirmed)
results = left_join(results, real, by = "date")
cols_proj =  c("Non linear regression model (MATGEN)" = "#999999",
               "Error correction model (MATGEN)" = "#6699CC",
               "SIR" = "#FF9966",
               "Auto Arima" = "#CC6699",
               "Neural Network" = "#66FF99")
results = results[results$date >= as.Date("2020-07-09"),]

## Puntuales --------

### Gráficos ------

results %>%
  filter(type == "prediccion") %>%
  ggplot() + 
  geom_point(aes(x = date, y = real)) +
  geom_line(aes(x = date, y = real)) +
  geom_point(aes(x = date, y = value, col = model)) + 
  geom_line(aes(x = date, y = value, col = model), size = 1) + 
  facet_wrap(~ last_date_train, scales = "free_x", ncol = 4) + 
  theme_light() + 
  ggtitle("Forecast of next 14 days",
          subtitle = "Different top dates to train") + 
  ylab("Number of people") + 
  xlab("") + 
  theme(text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  scale_color_manual(values = cols_proj) + 
  ylim(0, 11000)+
  scale_x_date(labels = scales::date_format("%d-%m")) 

ggplot2::ggsave(filename = "article_results/images/comparative_model_plots/puntual_forecast.png",
                height=8, width = 10)


results %>%
  filter(type == "ajuste") %>%
  ggplot() + 
  geom_line(aes(x = date, y = real)) +
  geom_line(aes(x = date, y = value, col = model), size = 1) + 
  facet_wrap(~ last_date_train, scales = "free_x", ncol = 4) + 
  theme_light() + 
  ggtitle("Adjusted models",
          subtitle = "Differents top dates to train") + 
  ylab("Number of people") + 
  xlab("") + 
  theme(text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90))+ 
  scale_color_manual(values = cols_proj)+ 
  ylim(0, 8000)+
  scale_x_date(labels = scales::date_format("%d-%m"))

ggplot2::ggsave(filename = "article_results/images/comparative_model_plots/puntual_adjusted.png",
                height=8, width = 10)

### Medidas de ajuste -----

results %>%
  mutate(
    type = ifelse(type == "ajuste", "Fitted", "Forecast")
  ) %>%
  group_by(last_date_train, model, type) %>%
  summarise(
    forecast::accuracy(real, value) %>% as.data.frame(),
    #R2 = cor(value, real)^2
    R2 = 1 - (mean((real - value)^2)/var(real))
  ) %>%
  mutate(
    MAPE = ifelse(model == "SIR" & type == "Fitted", NA, MAPE),
    MAPE = ifelse(MAPE > 150, NA, MAPE),
    MPE = ifelse(model == "SIR" & type == "Fitted", NA, MPE)
  ) %>%
  pivot_longer(c(-last_date_train, -model, -type), names_to = "metric") %>% 
  ggplot() + 
  geom_boxplot(aes(x =  type, y = value, fill = model)) + 
  facet_wrap( ~ metric, scales = "free") + 
  theme_light() +
  ylab("") + 
  xlab("") + 
  ggtitle("Comparing distribution of different metrics") + 
  scale_fill_manual(values = cols_proj)+ 
  theme(text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank())

ggplot2::ggsave(filename = "article_results/images/comparative_model_plots/puntual_metrics_comparision.png",
                height=6, width = 7)

# Tabla de métricas sobre el ajuste
results %>%
  mutate(
    type = ifelse(type == "ajuste", "Fitted", "Forecast")
  ) %>%
  group_by(last_date_train, model, type) %>%
  summarise(
    forecast::accuracy(real, value) %>% as.data.frame(),
    # R2 = cor(value, real)^2
    R2 = 1 - (mean((real - value)^2)/var(real))
  ) %>%
  ungroup() %>%
  filter(type == "Fitted") %>%
  dplyr::select(-type) %>%
  mutate(set = as.character(as.numeric(as.factor(last_date_train)))) %>%
  relocate(set, .before = last_date_train) %>%
  dplyr::select(-last_date_train) %>%
  xtable::xtable() %>%
  print(include.rownames=FALSE)

results %>%
  mutate(
    type = ifelse(type == "ajuste", "Fitted", "Forecast")
  ) %>%
  group_by(last_date_train, model, type) %>%
  summarise(
    forecast::accuracy(real, value) %>% as.data.frame(),
    # R2 = cor(value, real)^2
    R2 = 1 - (mean((real - value)^2)/var(real))
  ) %>%
  ungroup() %>%
  filter(type == "Forecast")%>%
  dplyr::select(-type) %>%
  mutate(set = as.character(as.numeric(as.factor(last_date_train)))) %>%
  relocate(set, .before = last_date_train) %>%
  dplyr::select(-last_date_train) %>%
  xtable::xtable() %>%
  print(include.rownames=FALSE)




# results %>%
#   group_by(last_date_train, model, type) %>%
#   summarise(
#     R2 = cor(value, real)^2,
#     R2_beatriz = 1 - (mean((real - value)^2)/var(real))
#   ) %>%
#   ggplot()+ 
#   geom_boxplot(aes(x = type, y = R2_beatriz, fill = model)) +
#   theme_light() +
#   ylab(TeX("R^{2}")) + 
#   xlab("") + 
#   theme(text = element_text(face = "bold")) + 
#   ggtitle(TeX("Comparing R^{2}"))

results %>%
  group_by(model, type) %>%
  summarise(
    forecast::accuracy(real, value) %>% as.data.frame(),
    R2 = cor(value, real)^2
    #R2 = 1 - (mean((real - value)^2)/var(real))
  ) %>%
  xtable::xtable()


## Acumuladas -------

### Gráficos ------

results %>%
  group_by(last_date_train, model) %>%
  arrange(date) %>%
  mutate_at(vars(value, real), .funs = ~ cumsum(.))%>%
  filter(type == "prediccion") %>%
  ggplot() + 
  geom_line(aes(x = date, y = real)) +
  geom_line(aes(x = date, y = value, col = model), size = 1) + 
  facet_wrap(~ last_date_train, scales = "free", ncol = 4) + 
  theme_light() + 
  ggtitle("Forecast of next 14 days",
          subtitle = "Different top dates to train") + 
  ylab("Number of people") + 
  xlab("") + 
  theme(text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90))+ 
  scale_color_manual(values = cols_proj) + 
  scale_x_date(labels = scales::date_format("%d-%m"))

ggplot2::ggsave(filename = "article_results/images/comparative_model_plots/cumulative_forecast.png",
                height=8, width = 10)


results %>%
  group_by(last_date_train, model) %>%
  arrange(date) %>%
  mutate_at(vars(value, real), .funs = ~ cumsum(.))%>%
  filter(type == "ajuste") %>%
  ggplot() + 
  geom_line(aes(x = date, y = real)) +
  geom_line(aes(x = date, y = value, col = model), size = 1) + 
  facet_wrap(~ last_date_train, scales = "free", ncol = 4) + 
  theme_light() + 
  ggtitle("Adjusted models",
          subtitle = "Differents top dates to train") + 
  ylab("Number of people") + 
  xlab("Dates") + 
  theme(text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90))+ 
  scale_color_manual(values = cols_proj) + 
  scale_x_date(labels = scales::date_format("%d-%m"))
ggplot2::ggsave(filename = "article_results/images/comparative_model_plots/cumulative_adjusted.png",
                height=8, width = 10)


### Medidas de ajuste ------

results %>% mutate(
  type = ifelse(type == "ajuste", "Fitted", "Forecast")
) %>%
  group_by(last_date_train, model) %>%
  arrange(date) %>%
  mutate_at(vars(value, real), .funs = ~ cumsum(.)) %>%
  group_by(last_date_train, model, type) %>%
  summarise(
    forecast::accuracy(real, value) %>% as.data.frame(),
    # R2 = cor(value, real)^2
    R2 = 1 - (mean((real - value)^2)/var(real))
  ) %>%
  mutate(
    MAPE = ifelse(model == "SIR" & type == "Fitted", NA, MAPE),
    MPE = ifelse(model == "SIR" & type == "Fitted", NA, MPE),
    R2 = ifelse(R2 < -0.5, NA, R2)
  ) %>%
  pivot_longer(c(-last_date_train, -model, -type), names_to = "metric") %>% 
  ggplot() + 
  geom_boxplot(aes(x =  type, y = value, fill = model)) + 
  facet_wrap( ~ metric, scales = "free") + 
  theme_light() +
  ylab("") + 
  xlab("") + 
  theme(text = element_text(face = "bold")) + 
  ggtitle("Comparing distribution of different metrics") +
  theme(text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank())+ 
  scale_fill_manual(values = cols_proj)

ggplot2::ggsave(filename = "article_results/images/comparative_model_plots/cumulative_metrics_comparision.png",
                height=6, width = 7)

# Ajuste
results %>% mutate(
  type = ifelse(type == "ajuste", "Fitted", "Forecast")
) %>%
  group_by(last_date_train, model) %>%
  arrange(date) %>%
  mutate_at(vars(value, real), .funs = ~ cumsum(.)) %>%
  group_by(last_date_train, model, type) %>%
  summarise(
    forecast::accuracy(real, value) %>% as.data.frame(),
    # R2 = cor(value, real)^2
    R2 = 1 - (mean((real - value)^2)/var(real))
  ) %>%
  ungroup() %>%
  filter(type == "Fitted") %>%
  dplyr::select(-type) %>%
  mutate(set = as.character(as.numeric(as.factor(last_date_train)))) %>%
  relocate(set, .before = last_date_train) %>%
  dplyr::select(-last_date_train) %>%
  xtable::xtable() %>%
  print(include.rownames=FALSE)

# Prediccion
results %>% mutate(
  type = ifelse(type == "ajuste", "Fitted", "Forecast")
) %>%
  group_by(last_date_train, model) %>%
  arrange(date) %>%
  mutate_at(vars(value, real), .funs = ~ cumsum(.)) %>%
  group_by(last_date_train, model, type) %>%
  summarise(
    forecast::accuracy(real, value) %>% as.data.frame(),
    # R2 = cor(value, real)^2
    R2 = 1 - (mean((real - value)^2)/var(real))
  ) %>%
  ungroup() %>%
  filter(type == "Forecast") %>%
  dplyr::select(-type) %>%
  mutate(set = as.character(as.numeric(as.factor(last_date_train)))) %>%
  relocate(set, .before = last_date_train) %>%
  dplyr::select(-last_date_train) %>%
  xtable::xtable() %>%
  print(include.rownames=FALSE)

results %>%
  group_by(last_date_train, model) %>%
  arrange(date) %>%
  mutate_at(vars(value, real), .funs = ~ cumsum(.)) %>%
  group_by(last_date_train, model, type) %>%
  summarise(
    R2 = cor(value, real)^2,
    R2_beatriz = 1 - (mean((real - value)^2)/var(real))
  ) %>%
  ggplot()+ 
  geom_boxplot(aes(x = model, y = R2)) +
  facet_wrap(~ type, scales = "free_y") +
  theme_light() +
  ylab(TeX("R^{2}")) + 
  xlab("") + 
  theme(text = element_text(face = "bold")) + 
  ggtitle(TeX("Comparing R^{2}"))


results %>%
  group_by(last_date_train, model) %>%
  arrange(date) %>%
  mutate_at(vars(value, real), .funs = ~ cumsum(.)) %>%
  group_by(model, type) %>%
  summarise(
    forecast::accuracy(real, value) %>% as.data.frame(),
    R2 = 1 - (mean((real - value)^2)/var(real))
  ) %>% as.data.frame() %>%
  xtable::xtable()




## Picos -----

### Joses model ------
picos = lapply(dates_test, 
               function(x) {
                 dg = df[df$date <= x,]
                 dh = df[df$date >= as.Date("2020-06-24"),]
                 dh = dh[dh$date <= (x + lubridate::days(15)),]
                 model = ecm$new(
                   data = dg, K = params$K, r = params$r, seasonal = params$seasonal)
                 model$train()
                 pred = model$predict(h = 100)
                 pred$h = NULL
                 pred = bind_rows(dg[dg$date >=  as.Date("2020-06-24"), colnames(pred)], pred)  
                 data.frame(
                   dates = x,
                   prediccion_pico = pred$date[which.max(pred$confirmed)],
                   real = dh$date[which.max(dh$confirmed)]
                 )
                 
               })
picos = bind_rows(picos)
picos = arrange(picos, dates)


### AUTOARIMA ------

picos_auto = lapply(dates_test,
                    function(x) {
                      x = as.Date(x)
                      dg = df[df$date <= x,]
                      dh = df[df$date >= as.Date("2020-06-24"),]
                      dh = dh[dh$date <= (x + lubridate::days(15)),]
                      
                      auto_model = Arima(ts(dg$confirmed, frequency = 7),
                                         model = auto_model)
                      
                      pred = data.frame(
                        date = tail(seq.Date(from = x, by = "day", length.out = 101),-1),
                        confirmed = forecast(auto_model, 100)$mean %>% as.numeric())
                      pred = bind_rows(dg[dg$date >=  as.Date("2020-06-24"), colnames(pred)], pred)  
                      data.frame(
                        dates = x,
                        prediccion_pico_arima = pred$date[which.max(pred$confirmed)],
                        real = dh$date[which.max(dh$confirmed)]
                      )
                    })
picos_auto = bind_rows(picos_auto)


### SIR-----
resultados_sir = names(loutFinal) %>%
  lapply(function(x) {
    loutFinal[[x]] %>% 
      mutate(last_date_train = x) %>%
      mutate(
        type = ifelse(date <= as.Date(x), "ajuste", "prediccion")
      ) %>%
      dplyr::select(date, out_confirmed, type, last_date_train) %>%
      rename(value = out_confirmed)
  }) %>% bind_rows()


resultados_sir$model = "SIR"
resultados_sir$date = as.Date(resultados_sir$date)
resultados_sir$last_date_train = as.Date(resultados_sir$last_date_train)

picos_sir = resultados_sir %>%
  group_by(model, last_date_train) %>%
  slice(which.max(value)) %>%
  dplyr::select(-value)

picos_sir = picos_sir %>%
  ungroup() %>%
  rename(
    dates = last_date_train,
    fecha_estimada_sir = date
  ) %>%
  dplyr::select(dates, fecha_estimada_sir)

### Beatriz model-----

picos_beatriz = read.delim("article_results/ArticuloResultados/picos_beatriz_bien.txt", header = FALSE)
picos_beatriz = picos_beatriz[c("V1", "V2")]
colnames(picos_beatriz) = c("dates", "n_estimado_beatriz")
picos_beatriz$dates = lubridate::parse_date_time(picos_beatriz$dates, orders = "dmy")
picos_beatriz$dates = as.Date(picos_beatriz$dates - lubridate::days(1))
picos_beatriz$fecha_estimada_beatriz = as.Date(as.Date("2020-06-24") + lubridate::days(picos_beatriz$n_estimado_beatriz))
picos = left_join(picos, picos_beatriz, by = "dates")
picos = left_join(picos, picos_sir, by = "dates")
picos_auto$real = NULL
picos = left_join(picos, picos_auto, by = "dates")

### Comparativa ------

picos %>%
  dplyr::select(dates, real, prediccion_pico, fecha_estimada_beatriz,
                fecha_estimada_sir, prediccion_pico_arima) %>%
  rename(
    `Error correction model (MATGEN)` = prediccion_pico,
    `Non linear regression model (MATGEN)` = fecha_estimada_beatriz,
    SIR = fecha_estimada_sir,
    `Auto Arima` = prediccion_pico_arima) %>%
  tidyr::pivot_longer(c(-dates, -real), names_to = "Legend") %>%
  ggplot() + 
  # geom_point(aes(x = dates, y = real), size = 2) +
  # geom_line(aes(x = dates, y = real), size = 1, linetype = "dashed") +
  
  geom_point(aes(x = dates, y = value, col = Legend), size = 2) +
  geom_line(aes(x = dates, y = value, col = Legend), size = 1, linetype = "dashed") +
  ylab("Date of maximun value") + 
  xlab("Date of mesaure/forecast") + 
  theme_light() +
  ggtitle("Confirmed cases peak",
          subtitle = "Measuring the prediction of the peak of wave")+ 
  theme(
    text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) + 
  scale_color_manual(values = cols_proj) +
  geom_vline(xintercept = as.Date("2020-09-18"), size = 1) +
  geom_hline(yintercept = as.Date("2020-09-18", size = 1)) +
  geom_text(x = as.Date("2020-09-16"), y = as.Date("2020-10-30"),
            label = "Real peak: 2020-09-18", angle = 90,
            size = 4)

ggplot2::ggsave(filename = "article_results/images/comparative_model_plots/peak_prediction.png",
                height=10, width = 10)