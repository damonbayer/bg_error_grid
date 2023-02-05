library(tidyverse)
library(broom)
RiskPairData <- read_csv("https://github.com/mjfrigaard/seg-shiny-0.3.3/raw/master/App/AppRiskPairData.csv")

model <- lm(abs_risk ~ polym(REF, BGM, degree = 3), data = RiskPairData)

full_join(
  model %>%
    augment() %>%
    mutate(id = 1:n()),
  RiskPairData %>%
    mutate(id = 1:n())) %>%
  select(REF, BGM, abs_risk, .fitted) %>%
  pivot_longer(cols = c(abs_risk, .fitted), names_to = "source", values_to = "abs_risk") %>%
  ggplot(aes(REF, BGM, fill = abs_risk)) +
  facet_wrap(~source) +
  geom_raster(interpolate = F) +
  scale_fill_viridis_c() +
  coord_equal() +
  cowplot::theme_cowplot()
