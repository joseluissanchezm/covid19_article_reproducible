forecast_plot <- function(iso = NULL, horizon = 14, last_date = NULL, n_day_history = 0) {
  df = readRDS("data_app/historical.rds")
  x = df[df$iso == iso,]
  if (!is.null(last_date)) {
    x = x[x$date <= last_date,]
  }
  params = readRDS("data_app/params_isos.rds")
  params = params[params$iso == iso,]
  source("CODE/functions/models_definition.R")
  model = ecm$new(
    data = x, K = params$K, r = params$r, seasonal = params$seasonal)
  model$train()
  pred = model$predict(h = horizon)
  pred = mutate_at(pred, vars(confirmed, hosp, icu, deaths), .funs = ~ pmax(0, .))
  pred = tidyr::pivot_longer(pred, c(-date, -h))
  x$testing_policy = NULL
  x = pivot_longer(df[df$iso == iso,], c(confirmed, hosp, icu, deaths), names_to = "name", values_to = "value")
  x$iso = NULL
  x$type = "Histórico"
  pred$type = "Predicción"
  pred$h = NULL
  x$testing_policy = NULL
  x = x[x$date <= max(pred$date),]
  x = tail(x, (n_day_history + horizon)*4)
  pred = bind_rows(x, pred)
  
  ggplotly(ggplot(pred) + 
             geom_line(aes(x = date, y = value, col = type), size = 1.1, alpha = 0.6) + 
             facet_wrap(~ name, scales = "free_y", ncol = 2) + 
             theme_light() + 
             theme(
               text = element_text(face = "bold"),
               legend.title = element_blank(),
               legend.position="bottom"
             ) + 
             ylab("Número de personas") + 
             xlab("Fecha") +
             scale_color_manual(values = c("Predicción" = "green", "Histórico" = "grey")))
}
