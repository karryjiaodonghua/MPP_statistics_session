
# install.packages(c("tidyverse", "here", "randomizr", "estimatr", "sf","geosphere","whisker","leaflet"))
library(tidyverse)
library(here)
library(randomizr)
library(estimatr)
library(sf)
library(leaflet)
librar(geosphere)


census <- readRDS(here("Dataworks","DataSets","Final", "census.RDS"))

# Random sampling -----------------------------------------------------
# A seed makes the sequence reproducible with the same code and RNG settings.
set.seed(14)
sample(x = 1:10, size = 4, replace = FALSE)
sample(x = letters, size = 3, replace = FALSE)

# With replacement, the same value may be selected more than once.
# replicate() repeats an expression; here it creates five sampled columns.
set.seed(14)
df1 <- data.frame(value = sample(1:20, 30, replace = TRUE))
df2 <- data.frame(replicate(5, sample(1:20, 30, replace = TRUE)))

# Inspect the census data: one row per state.
c(n_rows = nrow(census), n_states = n_distinct(census$state))
census %>% select(state, region, pop, medage) %>% head(5)

# Draw 10 distinct row positions, then retain selected columns.
set.seed(14)
selected_rows <- sample(seq_len(nrow(census)), size = 10, replace = FALSE)
census_sample <- census %>%
  slice(selected_rows) %>%
  select(state, region, pop, medage)
census_sample

# Random treatment assignment ----------------------------------------

set.seed(14)
census_sample <- census_sample %>%
  ungroup() %>%
  mutate(treat = sample(c(0, 1), size = n(), replace = TRUE,
                        prob = c(0.5, 0.5)))
census_sample %>% count(treat)

# Complete randomization: shuffle 25 zeros and 25 ones, rep(..., each = 25) repeats each value 25 times.

set.seed(14)
census_rct <- census_rct %>%
  mutate(treat = sample(rep(c(0, 1), each = 25), replace = FALSE))
census_rct %>% count(treat)
census_rct %>% filter(treat == 1) %>% select(state)
census_rct %>% filter(treat == 0) %>% select(state)

# A factor represents categories; levels and labels correspond in order.
census_rct <- census_rct %>%
  mutate(group = factor(treat, levels = c(0, 1),
                        labels = c("Control", "Treatment")))

# Package alternative: same design, potentially different realized assignment.
set.seed(14)
census_rct2 <- census_rct %>%
  select(-treat, -group) %>%
  mutate(treat = complete_ra(N = n(), m = 25),
         group = factor(treat, levels = c(0, 1),
                        labels = c("Control", "Treatment")))
census_rct2 %>% count(group)

# Comparing groups and estimating uncertainty -------------------------

census_rct <- census_rct %>% mutate(marg_rate = marriage / pop)
census_rct %>%
  summarise(difference = mean(marg_rate[treat == 1]) -
                         mean(marg_rate[treat == 0]))


fit_ols <- lm(marg_rate ~ treat, data = census_rct)
coef(fit_ols)

fit_robust <- lm_robust(marg_rate ~ treat, data = census_rct,
                        se_type = "HC2")
summary(fit_robust)

# Data joins ----------------------------------------------------------
set.seed(14)
a <- tibble(id = c(1, 2, 3, 4),
            score = sample(1:10, 4, replace = FALSE))
b <- tibble(id = c(2, 3, 5), region = c("East", "West", "Nowhere"))


left_join(a, b, by = "id")   # Keep IDs 1, 2, 3, 4.
right_join(a, b, by = "id")  # Keep IDs 2, 3, 5.
inner_join(a, b, by = "id")  # Keep IDs 2, 3.
full_join(a, b, by = "id")   # Keep IDs 1, 2, 3, 4, 5.


merge(a, b, by = "id", all = FALSE)  # Inner join (default).
merge(a, b, by = "id", all = TRUE)   # Full join.
merge(a, b, by = "id", all.x = TRUE) # Left join.
merge(a, b, by = "id", all.y = TRUE) # Right join.


# Geospatial data -------------------------------------------

country_sf <- st_read(here("Dataworks","DataSets","Final","country.geojson"))
head(country_sf)
nrow(country_sf)
st_crs(country_sf) # Coordinate reference system (CRS).
names(country_sf)

ggplot() + geom_sf(data = country_sf)

st_area(country_sf)
city_sf <- city_sf %>%
  mutate(area_m = city_sf %>% st_area() %>% as.numeric(),
         area_km = area_m / 1000^2)

# Plotting
ggplot() +
  geom_sf(data = city_sf,
          aes(fill = area_km)) +
  labs(fill = "Area") +
  scale_fill_distiller(palette = "Blues") + 
  theme_void()

# road lines -----------------------------------------
ggplot() +
  geom_sf(data = roads_sf,
          aes(color = highway)) +
  theme_void() +
  labs(color = "Road Type")