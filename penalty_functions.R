bg_risk_kovatchev <- function(bg) 10 * (1.509 * (log(pmax(bg, 1))^1.084 - 5.381))^2
bg_risk_bayer_kovatchev <- function(bg, target) 33.5200151972786 * (log(pmax(bg, 1)) - log(112.5))^2
bg_risk_bayer <- function(bg, target = 105) 32.9170208165394 * (log(pmax(bg, 1)) - log(target))^2

bg_risk_cao <- function(bg, bounded = T) {
  if (bounded) {
    case_when(
      50 <= bg & bg <= 80 ~ 1.0567 * (80 - bg)^1.3378,
      80 < bg & bg <= 140 ~ 0,
      140 < bg & bg <= 300 ~ 0.4607 * (bg - 140)^1.0601,
      TRUE ~ 100
    )
  } else {
    case_when(
      bg <= 80 ~ 1.0567 * (80 - bg)^1.3378,
      80 < bg & bg <= 140 ~ 0,
      140 < bg ~ 0.4607 * (bg - 140)^1.0601
    )
  }
}

bg_risk_van_herpe <- function(bg, bounded = T) {
  if (bounded) {
    case_when(
      20 <= bg & bg < 80 ~ 7.4680 * (80 - bg)^0.6337,
      80 <= bg & bg <= 110 ~ 0,
      110 < bg & bg <= 250 ~ 6.1767 * (bg - 110)^0.5635,
      TRUE ~ 100
    )
  } else {
    case_when(
      bg < 80 ~ 7.4680 * (80 - bg)^0.6337,
      80 <= bg & bg <= 110 ~ 0,
      110 < bg ~ 6.1767 * (bg - 110)^0.5635
    )
  }
}
