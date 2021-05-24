df = read.delim("article_results/ArticuloResultados/figura8beatriz.txt")
colnames(df) = c("dates", "mu0", "mu1", "XX", "sigma_left", "sigma_right", "X1", "n")
df$dates = lubridate::parse_date_time(df$dates, orders = "dmy")
df$dates = as.Date(df$dates - lubridate::days(1))
df$fecha_estimada_beatriz = as.Date(as.Date("2020-06-24") + lubridate::days(df$mu1))
library(dplyr)
library(ggplot2)
library(latex2exp)

pl1 = df %>%
  dplyr::select(dates, fecha_estimada_beatriz) %>%
  ggplot() + 
  geom_point(aes(x = dates, y = fecha_estimada_beatriz), size = 2) +
  geom_line(aes(x = dates, y = fecha_estimada_beatriz), size = 1) +
  geom_hline(yintercept = as.Date("2020-09-18"), col = "grey", size = 1) +
  ylab("") + 
  xlab("Predicted date") + 
  theme_light() +
  ggtitle("Confirmed cases peak",
          subtitle = "Measuring the prediction of the peak of wave")+ 
  theme(
    text = element_text(face = "bold"),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) + 
  geom_vline(xintercept = as.Date("2020-09-18"), size = 1, col = "grey") +
  geom_text(x = as.Date("2020-09-16"), y = as.Date("2020-09-05"),
            label = "2020-09-18", angle = 90,
            size = 4, col = "grey") +
  geom_text(x = as.Date("2020-11-16"), y = as.Date("2020-09-17"),
            label = "2020-09-18",
            size = 4, col = "grey")



pl2 = df %>%
  dplyr::select(dates, sigma_left, sigma_right) %>%
  mutate_at(vars(-dates),.funs = ~ stringr::str_replace_all(., ",", ".")) %>%
  mutate_at(vars(-dates), .funs = ~ as.numeric(.)) %>%
  tidyr::pivot_longer(-dates, names_to = "sigma", values_to = "value") %>%
  ggplot() + 
  geom_point(aes(x = dates, y = value, col = sigma), size = 2) + 
  geom_line(aes(x = dates, y = value, col = sigma), size = 1) + 
  ylab("") + 
  xlab("Date") + 
  theme_light() +
  theme(
    text = element_text(face = "bold"),
    legend.title = element_blank()
  ) + 
  ggtitle(expression(sigma~variation~over~time)) + 
  scale_color_manual(values = c("sigma_left" = "darkgrey",
                                "sigma_right" = "black")) + 
  geom_vline(xintercept = as.Date("2020-09-18"), size = 1, col = "grey") +
  geom_text(x = as.Date("2020-09-16"), y = 7,
            label = "2020-09-18", angle = 90,
            size = 4, col = "grey") 


pl3 = df %>%
  dplyr::select(dates, n) %>%
  mutate_at(vars(-dates), .funs = ~ as.numeric(.)) %>%
  ggplot() + 
  geom_point(aes(x = dates, y = n), size = 2) + 
  geom_line(aes(x = dates, y = n), size = 1) + 
  ylab("N") + 
  xlab("Date") + 
  theme_light() +
  theme(
    text = element_text(face = "bold"),
    legend.title = element_blank()
  ) + 
  geom_vline(xintercept = as.Date("2020-09-18"), size = 1, col = "grey") +
  geom_text(x = as.Date("2020-09-16"), y = 130000,
            label = "2020-09-18", angle = 90,
            size = 4, col = "grey") + 
  scale_y_continuous(labels = scales::comma) + 
  ggtitle("N prediction variation over time")


library(cowplot)
plot_grid(pl1, pl2, pl3, ncol = 1, align = "v", axis = "l")            
