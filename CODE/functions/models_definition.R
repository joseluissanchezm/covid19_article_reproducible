# Modelos finales para decidir en total_spain
library(R6)
library(dplyr)
library(vars)


ecm = R6Class("ecm",
                 public = list(
                   data = NULL,
                   exogen = NULL,
                   test = NULL,
                   model = NULL,
                   fitted = NULL,
                   residuals = NULL,
                   type = "trace",
                   ecdet = "none",
                   spec = "transitory",
                   seasonal = NULL,
                   K = 8,
                   r = 2,
                   initialize = function(data, K = 8, r = 2, seasonal = FALSE) {
                     self$data = data
                     self$K = K
                     self$r = r
                     if (seasonal) {
                       self$seasonal = 7
                     }
                     self$exogen = private$generate_exogen(self$data)
                   }
                 ))


ecm$set("private", "generate_exogen",
           function(data) {
             self$exogen = data["testing_policy"] %>%
               fastDummies::dummy_cols("testing_policy", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
             
           })

ecm$set("public", "train",
           function() {
             self$test = ca.jo(
               x = self$data[c("confirmed", "hosp", "icu", "deaths")] %>%
                 mutate_all(.funs = ~ log(. + 1)),
               type = self$type,
               ecdet = self$ecdet,
               spec = self$spec,
               K = self$K,
               season = self$seasonal,
               dumvar = self$exogen
             )
             self$model = vec2var(self$test, r = self$r)
             
             
             self$fitted = self$model %>% 
               fitted() %>% 
               as.data.frame() %>%
               rename(
                 confirmed = `fit of confirmed`,
                 hosp = `fit of hosp`,
                 icu = `fit of icu`,
                 deaths = `fit of deaths`
               ) %>%
               mutate_all(.funs = ~ exp(.) - 1) %>%
               mutate_all(.funs = ~ pmax(0,.)) %>%
               mutate(
                 date = tail(self$data$date, nrow(.))
               )
             
             self$residuals = data.frame(
               date = self$fitted$date,
               confirmed = tail(self$data$confirmed, nrow(self$fitted)) - self$fitted$confirmed ,
               hosp = tail(self$data$hosp, nrow(self$fitted)) - self$fitted$hosp ,
               icu = tail(self$data$icu, nrow(self$fitted)) - self$fitted$icu ,
               deaths = tail(self$data$deaths, nrow(self$fitted)) - self$fitted$deaths
             )
             
           })


ecm$set("public", "predict",
           function(h = 15, type = "puntual") {
             future_exogen = tail(self$exogen,1) %>% slice(rep(1:n(), each = h))
             
             pred = predict(self$model,
                            n.ahead = h, 
                            dumvar = as.matrix(future_exogen))
             pred = pred$fcst %>% 
               lapply(function(x) x[,1]) %>%
               bind_cols()
             pred = mutate_all(pred, .funs = ~ exp(.) - 1)
             pred$date = seq.Date(from = max(self$data$date), by = "day",
                                  length.out = h + 1) %>% tail(h)
             
             if (type == "cummulative") {
               pred = bind_rows(self$data[colnames(pred)], pred)
               pred = pred %>%
                 arrange(date) %>%
                 mutate_at(vars(-date), .funs = ~ cumsum(.)) %>% 
                 tail(h)
             }
             
             pred$h = 1:h
             return(pred)

           })


