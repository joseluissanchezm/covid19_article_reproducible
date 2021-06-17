library(forecastML)
iso = "MD"
last_date_use = as.Date("2021-04-01")
df = readRDS("data_app/historical.rds")
params = readRDS("data_app/params_isos.rds")
df = df[df$iso == iso,]
params = params[params$iso == iso,]

data_train <- df %>% filter(date < "2020-08-01")
dates = data_train$date
data_train = data_train[c("confirmed")]
outcome_col <- 1  # The column index of our DriversKilled outcome.

horizons <- c(1, 3, 7, 10, 15)  # 4 models that forecast 1, 1:3, 1:6, and 1:12 time steps ahead.

# A lookback across select time steps in the past. Feature lags 1 through 9, for instance, will be 
# silently dropped from the 12-step-ahead model.
lookback <- c(1:6, 9, 12,14, 15)





data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = "day"
)
windows <- forecastML::create_windows(lagged_df = data_list, window_length = 15,
                                      window_start = NULL, window_stop = NULL,
                                      include_partial_window = TRUE)
windows

model_function_2 <- function(data) {
  
  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names,  "~ ."))
  
  model <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
  return(model)
}

model_results_2 <- forecastML::train_model(data_list, windows, model_name = "RF", 
                                           model_function_2, use_future = FALSE)


prediction_function_2 <- function(model, data_features) {
  
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}
data_results <- predict(model_results_2,
                        prediction_function = list( prediction_function_2), 
                        data = data_list)


# Optimizacion del modelo

data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = "day"
)
windows <- forecastML::create_windows(data_list, window_length = 0)

model_results <- forecastML::train_model(data_list, windows,  model_name = "RF", model_function_2)

data_results <- predict(model_results, prediction_function = list(prediction_function_2), data = data_list)

data_forecast_list <- forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = "day"
)

data_forecast <- predict(model_results, prediction_function = list(prediction_function_2),
                         data = data_forecast_list)

data_combined <- forecastML::combine_forecasts(data_forecast)





forecast_rf = function(df) {
  dates = df$date
  x = df[c("confirmed")]
  data_list <- forecastML::create_lagged_df(x,
                                            outcome_col = outcome_col,
                                            type = "train",
                                            horizons = horizons,
                                            lookback = lookback,
                                            date = dates[1:nrow(x)],
                                            frequency = "day"
  )
  windows <- forecastML::create_windows(data_list, window_length = 0)
  
  model_results <- forecastML::train_model(data_list, windows,  model_name = "RF", model_function_2)
  
  data_results <- predict(model_results, prediction_function = list(prediction_function_2), data = data_list)
  
  data_forecast_list <- forecastML::create_lagged_df(x,
                                                     outcome_col = outcome_col,
                                                     type = "forecast",
                                                     horizons = horizons,
                                                     lookback = lookback,
                                                     date = dates[1:nrow(x)],
                                                     frequency = "day"
  )
  
  data_forecast <- predict(model_results, prediction_function = list(prediction_function_2),
                           data = data_forecast_list)
  
  data_combined <- forecastML::combine_forecasts(data_forecast)
  
  
  list(
    fitted = data_results[data_results$model_forecast_horizon == 1,] %>%
      dplyr::select(date_indices, confirmed_pred) %>%
      rename(
        date = date_indices,
        confirmed = confirmed_pred
      ),
    forecast = data_combined %>%
      rename(date = forecast_period,
             confirmed = confirmed_pred) %>%
      dplyr::select(date, confirmed)
  )
}


dates_test = test_sets$last_date_train

results_rf = lapply(dates_test,
                      function(date) {
                        dg = df[df$date <= date,]
                        
                        model = forecast_rf(dg)
                        
                        # Fitted series
                        fitted = model$fitted[c("date", "confirmed")]
                        fitted = filter(fitted, date >= as.Date("2020-06-24"))
                        fitted$last_date_train = date
                        fitted$type = "ajuste"
                        fitted = rename(fitted, value = confirmed)
                        # Puntual predict
                        punt_pred = model$forecast
                        punt_pred$last_date_train = date
                        punt_pred$type = "prediccion"
                        punt_pred = punt_pred[c("date", "confirmed", "last_date_train", "type")]
                        punt_pred = rename(punt_pred, value = confirmed)
                        
                        return(bind_rows(fitted, punt_pred))
                      })
results_rf = bind_rows(results_rf)
results_rf$model = "Direct Strategy - Random Forest"
results_rf
results

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
  ylim(0, 11000)+
  scale_x_date(labels = scales::date_format("%d-%m")) 



# XGBOOST -----

model_function <- function(data, outcome_col = 1) {
  
  # xgboost cannot handle missing outcomes data.
  data <- data[!is.na(data[, outcome_col]), ]
  
  indices <- 1:nrow(data)
  
  set.seed(224)
  train_indices <- sample(1:nrow(data), ceiling(nrow(data) * .8), replace = FALSE)
  test_indices <- indices[!(indices %in% train_indices)]
  
  data_train <- xgboost::xgb.DMatrix(data = as.matrix(data[train_indices, 
                                                           -(outcome_col), drop = FALSE]),
                                     label = as.matrix(data[train_indices, 
                                                            outcome_col, drop = FALSE]))
  
  data_test <- xgboost::xgb.DMatrix(data = as.matrix(data[test_indices, 
                                                          -(outcome_col), drop = FALSE]),
                                    label = as.matrix(data[test_indices, 
                                                           outcome_col, drop = FALSE]))
  
  params <- list("objective" = "reg:linear")
  watchlist <- list(train = data_train, test = data_test)
  
  set.seed(224)
  model <- xgboost::xgb.train(data = data_train, params = params, 
                              max.depth = 8, nthread = 2, nrounds = 30,
                              metrics = "rmse", verbose = 0, 
                              early_stopping_rounds = 5, 
                              watchlist = watchlist)
  
  return(model)
}
prediction_function <- function(model, data_features) {
  x <- xgboost::xgb.DMatrix(data = as.matrix(data_features))
  data_pred <- data.frame("y_pred" = predict(model, x),
                          "y_pred_lower" = predict(model, x) - 2,  # Optional; in practice, forecast bounds are not hard coded.
                          "y_pred_upper" = predict(model, x) + 2)  # Optional; in practice, forecast bounds are not hard coded.
  return(data_pred)
}


forecast_xgboost = function(df) {
  dates = df$date
  x = df[c("confirmed")]
  data_list <- forecastML::create_lagged_df(x,
                                            outcome_col = outcome_col,
                                            type = "train",
                                            horizons = horizons,
                                            lookback = lookback,
                                            date = dates[1:nrow(x)],
                                            frequency = "day"
  )
  windows <- forecastML::create_windows(data_list, window_length = 0)
  
  model_results <- forecastML::train_model(data_list, windows,  model_name = "xgboost", model_function)
  
  data_results <- predict(model_results, prediction_function = list(prediction_function), data = data_list)
  
  data_forecast_list <- forecastML::create_lagged_df(x,
                                                     outcome_col = outcome_col,
                                                     type = "forecast",
                                                     horizons = horizons,
                                                     lookback = lookback,
                                                     date = dates[1:nrow(x)],
                                                     frequency = "day"
  )
  
  data_forecast <- predict(model_results, prediction_function = list(prediction_function),
                           data = data_forecast_list)
  
  data_combined <- forecastML::combine_forecasts(data_forecast)
  
  
  list(
    fitted = data_results[data_results$model_forecast_horizon == 1,] %>%
      dplyr::select(date_indices, confirmed_pred) %>%
      rename(
        date = date_indices,
        confirmed = confirmed_pred
      ),
    forecast = data_combined %>%
      rename(date = forecast_period,
             confirmed = confirmed_pred) %>%
      dplyr::select(date, confirmed)
  )
}


dates_test = test_sets$last_date_train

results_xgboost = lapply(dates_test,
                    function(date) {
                      dg = df[df$date <= date,]
                      
                      model = forecast_xgboost(dg)
                      
                      # Fitted series
                      fitted = model$fitted[c("date", "confirmed")]
                      fitted = filter(fitted, date >= as.Date("2020-06-24"))
                      fitted$last_date_train = date
                      fitted$type = "ajuste"
                      fitted = rename(fitted, value = confirmed)
                      # Puntual predict
                      punt_pred = model$forecast
                      punt_pred$last_date_train = date
                      punt_pred$type = "prediccion"
                      punt_pred = punt_pred[c("date", "confirmed", "last_date_train", "type")]
                      punt_pred = rename(punt_pred, value = confirmed)
                      
                      return(bind_rows(fitted, punt_pred))
                    })
results_xgboost = bind_rows(results_xgboost)
results_xgboost$model = "Direct Strategy - xgboost"
results_xgboost
real = df %>% dplyr::select(date, confirmed) %>% rename(real = confirmed)
results =bind_rows(results_rf, results_xgboost)

library(ggplot2)

results = results[results$last_date_train != "2020-08-01",]

real = df %>% dplyr::select(date, confirmed) %>% rename(real = confirmed)
results = left_join(results, real, by = "date")


results %>%
  filter(type == "prediccion") %>%
  ggplot() + 
  geom_point(aes(x = date, y = real), col = "black") +
  geom_line(aes(x = date, y = real), col = "black") +
  geom_point(aes(x = date, y = value, col = model)) + 
  geom_line(aes(x = date, y = value, col = model), size = 1) + 
  facet_wrap(~ last_date_train, scales = "free_x", ncol = 4) + 
  theme_light() + 
  ggtitle("Forecast of next 14 days",
          subtitle = "Different top dates to train") + 
  ylab("") + 
  xlab("") + 
  theme(text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  scale_x_date(labels = scales::date_format("%d-%m")) 

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



results %>%
  group_by(model, type) %>%
  summarise(
    forecast::accuracy(real, value) %>% as.data.frame(),
    R2 = cor(value, real)^2
    #R2 = 1 - (mean((real - value)^2)/var(real))
  ) %>%
  xtable::xtable()
