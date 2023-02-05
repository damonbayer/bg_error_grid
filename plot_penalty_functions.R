library(tidyverse)
library(fs)
library(cowplot)
source("penalty_functions.R")
theme_set(theme_minimal_grid())

penalty_tbl <-
  tibble(bg = 20:600) %>%
  mutate(
    kovatchev = bg_risk_kovatchev(bg),
    bayer = bg_risk_bayer(bg),
    cao = bg_risk_cao(bg),
    van_herpe = bg_risk_van_herpe(bg),
    cao_unbounded = bg_risk_cao(bg, bounded = F),
    van_herpe_unbounded = bg_risk_van_herpe(bg, bounded = F)
  ) %>%
  pivot_longer(-bg, names_to = "method", values_to = "risk") %>%
  group_by(method) %>%
  mutate(normalized_risk = risk / max(risk) * 100)

penalty_functions_plot <-
  penalty_tbl %>%
  ggplot(aes(bg, normalized_risk, color = method)) +
  geom_line() +
  scale_y_continuous("Risk") +
  scale_x_continuous("Blood Glucose (mg/dL)") +
  scale_color_discrete("Method", labels = ~ str_to_title(str_replace_all(., "_", " "))) +
  theme_minimal_grid() +
  theme(legend.position = "bottom")

save_plot(
  filename = path("figures", "penalty_functions_plot", ext = "png"),
  plot = penalty_functions_plot
)
