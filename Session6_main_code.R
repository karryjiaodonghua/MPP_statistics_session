
# Session 6 code: DID and RDD

library(knitr)
library(tidyverse)
library(hrbrthemes)
library(fontawesome)
library(here)
library(xaringanExtra)
library(janitor)
library(modelsummary)
library(fixest)
library(kableExtra)
library(rdrobust)
library(sandwich)
library(lmtest)



##----------------------------------------------------------
# 1: DID

housing <- read_csv(here("DataWork", "DataSets", "Final","diff-in-diff-housing-example.csv"))

housing <- housing %>%
  rename(
    price = House_Price,
    treat = Group,
    post = Post_Treatment
  )

# treat: 1 = treated group; 0 = comparison group
# post:  1 = after treatment; 0 = before treatment

head(housing)

housing %>% count(treat, post)

mean_prices <- housing %>%
  group_by(treat, post) %>%
  summarise(
    n = n(),
    mean_price = mean(price),
    .groups = "drop"
  )


##----------------------------------------------------------
# Present summary statistics 

housing %>%
  mutate(
    Group = factor(
      treat,
      levels = c(0, 1),
      labels = c("Comparison", "Treated")
    ),
    Period = factor(
      post,
      levels = c(0, 1),
      labels = c("Before", "After")
    )
  ) %>%
  datasummary(
    Group * Period ~ price * (N + Mean),
    data = .,
    fmt = 2
  )

##----------------------------------------------------------
# Plot the observed group changes 

ggplot(
  mean_prices,
  aes(
    x = post,
    y = mean_price,
    colour = factor(treat)
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Before", "After")
  ) +
  scale_colour_manual(
    values = c("#4787B5", "#B22222"),
    labels = c("Comparison", "Treated")
  ) +
  labs(
    x = NULL,
    y = "Mean price",
    colour = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom")

##----------------------------------------------------------
# Calculate before-after changes 

price_change <- mean_prices %>%
  select(treat, post, mean_price) %>%
  pivot_wider(
    names_from = post,
    values_from = mean_price,
    names_prefix = "period_"
  )

price_change <- price_change %>%
  rename(
    post = period_1,
    before = period_0
  ) %>%
  mutate(price_diff = post - before)

price_change


# Calculate DiD manually 

with(price_change,
  price_diff[treat == 1] - price_diff[treat == 0])


##----------------------------------------------------------
# Regress group-average changes on treatment group 

did_price_change_lm <- lm(
  price_diff ~ treat,
  data = price_change
)

coef(did_price_change_lm)
summary(did_price_change_lm)

# This dataset has only TWO group-level rows. Two coefficients leave no residual degrees of freedom.


##----------------------------------------------------------
# Estimate DiD using the original observations

did_lm <- lm(
  log(price) ~ treat * post,
  data = housing
)

did_foels <- feols(
  price ~ treat * post,
  data = housing,
  vcov = "hetero"
)

modelsummary(
  list("lm()" = did_lm, "feols()" = did_foels),
  statistic = c(
    "SE = {std.error}",
    "p = {p.value}"
  ),
  stars = TRUE,
  fmt = 3)

##----------------------------------------------------------
# Part 2: Regression Discontinuity


alcohol_raw <- read_csv(
  here(
    "DataWork", "DataSets", "Final",
    "RegDisc2.csv"
  )
)

head(alcohol_raw)

datasummary(
  agecell + all + alcohol + homicide + suicide +
    mva + drugs + externalother ~ N + Mean + SD + Min + Max,
  data = alcohol_raw,
  fmt = 2)


##----------------------------------------------------------
# Create treatment group and cutoff
rd_data <- alcohol_raw %>%
  drop_na(all, mva, alcohol, drugs) %>%
  mutate(
    Treatment = as.integer(agecell >= 21),
    age_c = agecell - 21
  )

rd_data %>%
  count(Treatment)


# regressions

reg1 <- lm(
  all ~ Treatment + age_c,
  data = rd_data
)

reg2 <- lm(
  mva ~ Treatment + age_c,
  data = rd_data
)

reg3 <- lm(
  alcohol ~ Treatment + age_c,
  data = rd_data
)

reg4 <- lm(
  drugs ~ Treatment + age_c,
  data = rd_data
)

reg_models <- list(
  "All causes" = reg1,
  "Car accidents" = reg2,
  "Alcohol" = reg3,
  "Drugs" = reg4
)

modelsummary(
  reg_models,
  statistic = c(
    "SE = {std.error}",
    "p = {p.value}"
  ),
  coef_map = c(
    "Treatment" = "Legal access at 21",
    "age_c" = "Centred age",
    "(Intercept)" = "Intercept"
  ),
  stars = TRUE,
  fmt = 3
  )



##Additional-------------------------------------
# Not required, just for graph demonstration
pred_grid <- expand_grid(
  Treatment = 0:1,
  age_c = seq(-2, 2, length.out = 401)
) %>%
  filter(
    (Treatment == 0 & age_c <= 0) |
      (Treatment == 1 & age_c >= 0)
  ) %>%
  mutate(
    agecell = age_c + 21,
    fitted = predict(reg2, newdata = .)
  )

ggplot(
  rd_data,
  aes(
    x = agecell,
    y = mva,
    colour = factor(Treatment)
  )
) +
  geom_point(size = 2) +
  geom_line(
    data = pred_grid,
    aes(
      x = agecell,
      y = fitted,
      colour = factor(Treatment),
      group = Treatment
    ),
    linewidth = 1
  ) +
  geom_vline(
    xintercept = 21,
    linetype = "dashed"
  ) +
  scale_colour_manual(
    values = c("#4787B5", "#B22222"),
    labels = c("Below 21", "21 and above")
  ) +
  labs(
    x = "Age",
    y = "Car-accident mortality rate",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 9)
  )
