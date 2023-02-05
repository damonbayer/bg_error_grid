library(tidyverse)
library(cowplot)
source("penalty_functions.R")
theme_set(theme_minimal_grid())
target_bg <- 105

simple_loss_grid <-
  expand_grid(
    true_bg = 40:400,
    derived_bg = 40:400
  ) %>%
  mutate(dose_effect = target_bg - derived_bg) %>%
  mutate(eventual_bg = true_bg + dose_effect) %>%
  mutate(eventual_bg = if_else(eventual_bg < 1, 1, eventual_bg)) %>%
  mutate(
    penalty_bayer = bg_risk_bayer(eventual_bg),
    penalty_kovatchev = bg_risk_kovatchev(eventual_bg),
    penalty_cao_unbounded = bg_risk_cao(eventual_bg, bounded = F),
    penalty_van_herpe_unbounded = bg_risk_van_herpe(eventual_bg, bounded = F)
  ) %>%
  pivot_longer(starts_with("penalty_"),
    names_to = "penalty_type",
    names_prefix = "penalty_",
    values_to = "loss"
  ) %>%
  group_by(penalty_type) %>%
  mutate(normalized_loss = loss / max(loss) * 100) %>%
  ungroup()

simple_loss_function_grid_plot <-
  simple_loss_grid %>%
  ggplot(aes(true_bg, derived_bg, fill = normalized_loss)) +
  facet_wrap(~penalty_type, labeller = as_labeller(~ str_replace_all(.x, "_", " ") %>% str_to_title())) +
  geom_raster(interpolate = T) +
  scale_fill_viridis_c("Normalized Loss", direction = -1) +
  coord_fixed() +
  scale_y_continuous("Derived Blood Glucose (mg/dL)") +
  scale_x_continuous("True Blood Glucose (mg/dL)") +
  ggtitle("Simple Loss Function",
    subtitle = "Based on Various Penalty Functions"
  )

# Save Figures ------------------------------------------------------------
save_plot(
  filename = path("figures", "simple_loss_function_grid_plot", ext = "png"),
  plot = simple_loss_function_grid_plot,
  ncol = 2, nrow = 2
)
