library(tidyverse)
library(fs)
library(cowplot)
source("penalty_functions.R")
theme_set(theme_minimal_grid())
target_bg <- 105

percent_effect_remaining <- function(time,
                                     peak_activity_time = 75,
                                     action_duration = 360,
                                     delay = 10) {
  time_after_delay <- time - delay
  tau <- peak_activity_time * (1 - peak_activity_time / action_duration) / (1 - 2 * peak_activity_time / action_duration)
  a <- 2 * tau / action_duration
  S <- 1 / (1 - a + (1 + a) * exp(-action_duration / tau))
  case_when(
    time <= 0 ~ 1,
    time >= action_duration ~ 0,
    TRUE ~ 1 - S * (1 - a) *
      ((time^2 / (tau * action_duration * (1 - a)) - time / tau - 1) * exp(-time / tau) + 1)
  )
}

calculate_treatment_effect <- function(treatment_delta,
                                       time = seq(0, 360, by = 1),
                                       carb_absorption_rate = 2,
                                       peak_activity_time = 75,
                                       action_duration = 360,
                                       delay = 10) {
  if (treatment_delta > 0) {
    minutes_to_recovery <- treatment_delta / 2
    dose_trace <- case_when(
      time <= 10 ~ 0,
      time >= 10 + minutes_to_recovery ~ treatment_delta,
      TRUE ~ (time - 10) * 2
    )
  } else {
    dose_trace <- (1 - percent_effect_remaining(
      time, peak_activity_time,
      action_duration, delay
    )) *
      treatment_delta
  }
  tibble(time = time, dose_trace = dose_trace)
}


scenario_tbl <-
  expand_grid(
    initial_bg_true = seq(40, 400, length.out = 109),
    initial_bg_derived = seq(40, 400, length.out = 109)
  ) %>%
  mutate(id = row_number(), .before = 1) %>%
  mutate(
    treatment_delta_true = target_bg - initial_bg_true,
    treatment_delta_derived = target_bg - initial_bg_derived
  )

# Cache dose effects for efficiency
treatment_delta_dose_delta_lookup <-
  scenario_tbl %>%
  select(starts_with("treatment_delta_")) %>%
  pivot_longer(everything(), values_to = "treatment_delta") %>%
  distinct(treatment_delta) %>%
  mutate(treatment_effect = map(
    treatment_delta,
    ~ calculate_treatment_effect(treatment_delta = .x)
  ))

full_dose_traces <-
  scenario_tbl %>%
  pivot_longer(starts_with("treatment_delta_"),
    names_to = "treatment_delta_type",
    names_prefix = "treatment_delta_",
    values_to = "treatment_delta"
  ) %>%
  left_join(treatment_delta_dose_delta_lookup) %>%
  unnest(treatment_effect) %>%
  mutate(dose_trace = dose_trace + initial_bg_true) %>%
  select(id, time, dose_trace, treatment_delta_type) %>%
  mutate(dose_trace = if_else(dose_trace < 1, 1, dose_trace))

risk_tbl <-
  full_dose_traces %>%
  mutate(
    penalty_bayer = bg_risk_bayer(dose_trace),
    penalty_kovatchev = bg_risk_kovatchev(dose_trace),
    penalty_cao_unbounded = bg_risk_cao(dose_trace, bounded = F),
    penalty_van_herpe_unbounded = bg_risk_van_herpe(dose_trace, bounded = F)
  ) %>%
  pivot_longer(starts_with("penalty_"),
    names_to = "penalty_type",
    names_prefix = "penalty_",
    values_to = "loss"
  ) %>%
  group_by(id, treatment_delta_type, penalty_type) %>%
  summarize(avg_loss = mean(loss), .groups = "drop") %>%
  pivot_wider(
    names_from = treatment_delta_type,
    values_from = avg_loss,
    names_prefix = "avg_loss_"
  ) %>%
  mutate(risk = avg_loss_derived - avg_loss_true) %>%
  select(-starts_with("avg_loss_")) %>%
  left_join(scenario_tbl) %>%
  select(starts_with("initial_bg_"), penalty_type, risk) %>%
  group_by(penalty_type) %>%
  mutate(risk = if_else(risk < 0, 0, risk)) %>%
  mutate(risk = risk + min(risk[risk > 0])) %>%
  mutate(normalized_risk = risk / max(risk)) %>%
  ungroup()

better_loss_function_grid_plot <-
  risk_tbl %>%
  ggplot(aes(initial_bg_true, initial_bg_derived, fill = normalized_risk)) +
  facet_wrap(~penalty_type, labeller = as_labeller(~ str_replace_all(.x, "_", " ") %>% str_to_title())) +
  geom_raster(interpolate = T) +
  scale_fill_viridis_c("Normalized Loss", direction = -1) +
  coord_fixed() +
  scale_y_continuous("Derived Blood Glucose (mg/dL)") +
  scale_x_continuous("True Blood Glucose (mg/dL)") +
  ggtitle("Loss Function",
    subtitle = "Based on Various Penalty Functions"
  )

# Single Scenario ---------------------------------------------------------
single_scenario_id <-
  scenario_tbl %>%
  filter(
    initial_bg_true == 90,
    initial_bg_derived == 140
  ) %>%
  pull(id)

single_scenario_example <-
  full_dose_traces %>%
  filter(id == single_scenario_id) %>%
  mutate(
    penalty_bayer = bg_risk_bayer(dose_trace),
    penalty_kovatchev = bg_risk_kovatchev(dose_trace),
    penalty_cao_unbounded = bg_risk_cao(dose_trace, bounded = F),
    penalty_van_herpe_unbounded = bg_risk_van_herpe(dose_trace, bounded = F)
  ) %>%
  pivot_longer(starts_with("penalty_"),
    names_to = "penalty_type",
    names_prefix = "penalty_",
    values_to = "penalty"
  ) %>%
  left_join(scenario_tbl)

single_scenario_example_avg_penalty <-
  single_scenario_example %>%
  select(treatment_delta_type, penalty_type, penalty) %>%
  group_by(treatment_delta_type, penalty_type) %>%
  summarize(
    avg_penalty = mean(penalty),
    .groups = "drop"
  ) %>%
  mutate(label = str_c(
    "Avg. Penalty = ",
    scales::comma(avg_penalty, accuracy = 0.001)
  ))

single_scenario_dose_trace_plot <-
  single_scenario_example %>%
  distinct(time, dose_trace, treatment_delta_type, initial_bg_true, initial_bg_derived) %>%
  ggplot(aes(time, dose_trace, color = treatment_delta_type)) +
  geom_line() +
  scale_y_continuous("Simulated Blood Glucose (mg/dL)") +
  scale_x_continuous("Time (minutes)", breaks = seq(0, 360, by = 60)) +
  scale_color_discrete("Treatment Decision Made Using", labels = str_to_title) +
  theme(legend.position = "bottom")

single_scenario_penalty_grid_plot <-
  single_scenario_example %>%
  ggplot(aes(time, penalty, color = treatment_delta_type)) +
  facet_wrap(~penalty_type, labeller = as_labeller(~ str_replace_all(.x, "_", " ") %>% str_to_title())) +
  geom_hline(
    data = single_scenario_example_avg_penalty,
    mapping = aes(
      yintercept = avg_penalty,
      color = treatment_delta_type
    ),
    linetype = "dashed",
    show.legend = F
  ) +
  geom_text(
    data = single_scenario_example_avg_penalty,
    mapping = aes(
      x = 60,
      y = avg_penalty,
      label = label
    ), vjust = -1,
    show.legend = F
  ) +
  geom_line() +
  ggtitle("Penalty of Simulated True Blood Glucose Traces",
    subtitle = str_c("True BG: ", single_scenario_example$initial_bg_true[1], " mg/dL, Derived BG: ", single_scenario_example$initial_bg_derived[1], " mg/dL")
  ) +
  scale_y_continuous("Penalty") +
  scale_x_continuous("Time (minutes)", breaks = seq(0, 360, by = 60)) +
  scale_color_discrete("Treatment Decision Made Using", labels = str_to_title) +
  theme(legend.position = "bottom")

# Save Figures ------------------------------------------------------------
save_plot(
  filename = path("figures", "better_loss_function_grid_plot", ext = "png"),
  plot = better_loss_function_grid_plot,
  ncol = 2, nrow = 2
)

save_plot(
  filename = path("figures", "single_scenario_dose_trace_plot", ext = "png"),
  plot = single_scenario_dose_trace_plot
)

save_plot(
  filename = path("figures", "single_scenario_penalty_grid_plot", ext = "png"),
  plot = single_scenario_penalty_grid_plot,
  ncol = 2, nrow = 2
)
