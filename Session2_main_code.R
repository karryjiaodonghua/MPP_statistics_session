# Packages and data for this session
# Install if needed
install.packages(c("tidyverse", "here", "janitor", "modelsummary"))

library(tidyverse)
library(here)
library(janitor)
library(modelsummary)

# RECAP: Loading a Dataset in R
whr15 <- read_csv(here("DataWork", "DataSets", "Raw", "Un WHR", "WHR2015.csv")) 
whr16 <- read_csv(here("DataWork", "DataSets", "Raw", "Un WHR", "WHR2016.csv")) 
whr17 <- read_csv(here("DataWork", "DataSets", "Raw", "Un WHR", "WHR2017.csv"))

# The janitor Package: Clean Column Names Easily
whr15 <- whr15 %>% 
  clean_names() #<<
whr16 <- whr16 %>% 
  clean_names()
whr17 <- whr17 %>% 
  clean_names()

#------------------------------------------------
# View(data)      # Open the dataset in a viewer
# class(data)     # Object type (data frame, list, etc.)
# dim(data)       # Number of rows & columns
# names(data)     # Variable names
# str(data)       # Structure of the dataset
# summary(data)   # Summary statistics
# head(data)      # First few observations
# tail(data)      # Last few observations

# Exploring Data with the Tidyverse
# glimpse(data)   # Compact overview of dataset

# Dimensions of your data
dim(whr15) 

# Keys and matching relationships
whr15 %>% count(country) %>% filter(n > 1)

# Filtering and sorting dplyr::filter
whr15 %>% filter(region == "Western Europe")


# Filtering and sorting dplyr::filter
whr15 %>% 
  filter(region == "Eastern Asia" | region == "North America") 

whr15 %>% 
  filter(region %in% c("Eastern Asia", "North America")) 

# Filtering and sorting missing cases
whr15 %>% 
  filter(!is.na(region)) %>% #<<
  head(5)


# Filtering
whr15 %>% 
  filter(!region %in% c("Eastern Asia", "North America")) %>%  #<<
  head(5)

# Creating variables
whr15 <- whr15 %>% 
  mutate(year = 2015)

whr16 <- whr16 %>% 
  mutate(year = 2016)

whr17 <- whr17 %>% 
  mutate(year = 2017)

# Appending dataframes
bind_rows(whr15, whr16, whr17)


# Appending and merging data sets
regions <- read_rds(here("DataWork", "DataSets", "Raw", "Un WHR", "regions.RDS"))


# Appending and merging data sets
# Left, right, inner and full joins
a <- tibble(id = c(1, 2), score = c(10, 20))
b <- tibble(id = c(2, 3), region = c("East", "West"))

# Appending and merging data sets
whr17 <- whr17 %>% #<<
  left_join(regions, by = "country") %>% #<<
  select(country, region, everything()) 

# Appending and merging data sets
whr17 %>% 
  filter(is.na(region))
  


# Correcting country names before join
whr17 <- read_csv(here("DataWork", "DataSets", "Raw", "Un WHR", "WHR2017.csv")) %>%
  clean_names() %>%
  mutate(year = 2017,
         country = recode(country,
           "Taiwan Province of China" = "Taiwan",
           "Hong Kong S.A.R., China" = "Hong Kong")) %>%
  left_join(regions, by = "country")

# Appending and merging data sets
vars_to_keep <- c("country", "region", "year", "happiness_rank", 
              "happiness_score", "economy_gdp_per_capita", 
              "health_life_expectancy", "freedom")

whr15 <- select(whr15, all_of(vars_to_keep))
whr16 <- select(whr16, all_of(vars_to_keep))
whr17 <- select(whr17, all_of(vars_to_keep))

whr_panel <- rbind(whr15, whr16, whr17)    # or bind_rows

# Checking the combined data
whr_panel %>% count(country, year) %>% filter(n > 1)
whr_panel %>% filter(is.na(region))
nrow(whr_panel)

regions %>% count(country) %>% filter(n > 1)
anti_join(whr17, regions, by = "country") # unmatched country keys

# Subsetting for the descriptive analysis
whr <- whr_panel %>%
  filter(year == 2015) %>%
  select(country, region, happiness_score,
         health_life_expectancy, freedom)

# Base R
whr <- subset(
  whr_panel,
  year == 2015,
  select = c(country, region,
    happiness_score,
    health_life_expectancy, freedom)
)

# Quick summary statistics
summary(whr)


# Summarizing continuous variables
summary(whr$happiness_score)

# Summarizing categorical variables

whr %>% tabyl(region)

# Summarizing categorical variables
whr_panel %>% tabyl(region, year)



# Exploring datasets with *modelsummary*
datasummary_skim(whr, type = "numeric", output = "kableExtra")

# For categorical data:
datasummary_skim(whr %>% select(region), type = "categorical", output = "kableExtra")

# Exploring datasets with *modelsummary*
datasummary(
  happiness_score + health_life_expectancy + freedom ~ N + Mean + SD + Median + Min + Max,
  data = whr, output = "kableExtra"
)



# Variance and standard deviation: Base R
x <- whr$happiness_score
var(x, na.rm = TRUE)  # sample variance
sd(x, na.rm = TRUE)   # sample standard deviation
sqrt(var(x, na.rm = TRUE)) # same as sd()

# Variance and standard deviation: dplyr
whr %>%
  summarise(
    n_observed = sum(!is.na(happiness_score)),
    mean_happiness = mean(happiness_score, na.rm = TRUE),
    median_happiness = median(happiness_score, na.rm = TRUE),
    variance_happiness = var(happiness_score, na.rm = TRUE),
    sd_happiness = sd(happiness_score, na.rm = TRUE)
  )

# Summaries by group
region_stats <- whr %>%
  group_by(region) %>%
  summarise(
    n_observed = sum(!is.na(happiness_score)),
    mean_happiness = mean(happiness_score, na.rm = TRUE),
    median_happiness = median(happiness_score, na.rm = TRUE),
    sd_happiness = sd(happiness_score, na.rm = TRUE),
    .groups = "drop"
  )


# Standard error and confidence interval
waiting <- c(12, 18, 20, 22, 24, 25, 28, 30, 35, 40)
x_mean <- mean(waiting)
x_sd <- sd(waiting)
x_n <- length(waiting)
x_se <- x_sd / sqrt(x_n)
c(mean = x_mean, SD = x_sd, SE = x_se)

c(lower = x_mean - 1.96 * x_se,
  upper = x_mean + 1.96 * x_se)

# p-value
effect <- 3   # percentage points
se_effect <- 1.54
z <- (effect - 0) / se_effect
p_value <- 2 * pnorm(-abs(z))
c(z = z, p_value = p_value)
c(lower = effect - 1.96 * se_effect,
  upper = effect + 1.96 * se_effect)

# Loops and map

# Map
# Round the values of the following vector
x <- c(1.2, 2.5, 9.1, 5.8)
map(x, round)    # Returns a list; same as x %>% map(round)
round(x)         # since R is vectorized, this also works


# Explicit loop
df <- whr %>% select(where(is.numeric))
means_loop <- numeric(ncol(df))

for (i in seq_along(df)) {
  means_loop[i] <- mean(df[[i]], na.rm = TRUE)
}

# Functional iteration
means_map <- map(df, mean, na.rm = TRUE)

# Return a numeric vector
means_dbl <- map_dbl(df, mean, na.rm = TRUE)
