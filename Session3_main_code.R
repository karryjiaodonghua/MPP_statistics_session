library(tidyverse)
library(here)
library(janitor)
library(modelsummary)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggthemes)

install.packages("RColorBrewer")
library(RColorBrewer)

# Load the WHR2016 dataset

WHR2016 <- read.csv(
  here("DataWork", "DataSets", "Raw", "Un WHR", "WHR2016.csv")
  )
# load whr_panel
whr_panel <- read_csv(
  here("DataWork","DataSets","Final","whr_panel.csv")
  )


# Clean the variable names
WHR2016 <- clean_names(WHR2016)


# Calculate correlation: `cor()`
cor(
  WHR2016$happiness_score,
  WHR2016$economy_gdp_per_capita
)

# Select three numeric variables
whr_vars <- WHR2016 %>%
  select(happiness_score, economy_gdp_per_capita,
         health_life_expectancy)

# Calculate a correlation matrix
cor(whr_vars, use = "complete.obs")


#---------------------------------------------
# Regression:
reg1 <- lm(happiness_score ~ economy_gdp_per_capita,
           data = WHR2016)

summary(reg1)


# Add a second explanatory variable
reg2 <- lm(
  happiness_score ~ economy_gdp_per_capita + health_life_expectancy,
  data = WHR2016
)
summary(reg2)

modelsummary(list("Without control variables" = reg1,
                  "With control variables" = reg2))


# ------------------------------------------------------------


# Plot with Base R

vars <- c("economy_gdp_per_capita", "happiness_score", "health_life_expectancy", "freedom")
whr_plot <- whr_panel %>%
  select(all_of(vars))

plot(whr_plot)

ggplot(data = whr_panel) +  
  geom_point(aes(x = happiness_score, y = economy_gdp_per_capita))

# or: 

ggplot(data = whr_panel, aes(x = happiness_score, y = economy_gdp_per_capita)) +
  geom_point()

# or: 

whr_panel %>%
  ggplot(aes(x = happiness_score, y = economy_gdp_per_capita)) +
  geom_point()

# or: 

whr_panel %>%
  ggplot() +
  geom_point(aes(x = freedom, y = economy_gdp_per_capita))

# add color base on the categorical data: 

whr_panel %>%
ggplot(
  aes(
    x = happiness_score,       
    y = economy_gdp_per_capita,
    color = region
  )
) + 
  geom_point()

# add color base on your preference:

whr_panel %>%
  ggplot(
    aes(
      x = happiness_score,
      y = economy_gdp_per_capita
    )
  ) +
  geom_point(color = "blue")

# add color base on a continuous variable: 
whr_panel %>%
  ggplot(
    aes(
      x = freedom, 
      y = economy_gdp_per_capita, 
      color = year
    )
  ) +
  geom_point()

# Turn that continuous variable to a factor/category

whr_panel %>%
  ggplot(
    aes(
      x = freedom, 
      y = economy_gdp_per_capita, 
      color = as.factor(year)
    )
  ) +
  geom_point()+
  labs(color="year")

# scatter plot shape

whr_panel %>%
  ggplot(aes(x = happiness_score, y = economy_gdp_per_capita)) +
  geom_point(shape = 5)

# add a line

whr_panel %>%
  ggplot(aes(x = happiness_score,  y = economy_gdp_per_capita)) +
  geom_point() +
  geom_smooth()

# facet_wrap(~ region) makes one panel per region.

whr_panel %>%
  ggplot(aes(x = happiness_score,  y = economy_gdp_per_capita)) +
  geom_point() +
  facet_wrap(~ region)

whr_panel %>%
  ggplot(
    aes(
      x = happiness_score,  
      y = economy_gdp_per_capita, 
      color = region
    )
  ) + 
  geom_point() +
  facet_grid(~year)

# create a dummy variable
whr_panel <- whr_panel %>%
  mutate(
    latam = region == "Latin America and Caribbean"
  )
whr_panel %>%
  filter(
      !is.na(latam) # Make sure that we don't include missing values in our graph
  ) %>%
  ggplot(
    aes(
      x = happiness_score, y = economy_gdp_per_capita,
      color = latam)
  ) + 
  geom_point()

# change the size of the scatter plot

whr_panel %>%
  filter(year == 2017) %>%
  ggplot(aes(x = happiness_score,  y = economy_gdp_per_capita)) +
  geom_point(aes(size = economy_gdp_per_capita))


# change the scale

ggplot(data = whr_panel,
       aes(x = happiness_score,
           y = economy_gdp_per_capita)) +
  geom_point()

ggplot(data = whr_panel,
       aes(x = happiness_score,
           y = economy_gdp_per_capita)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(0, 2, 4, 6, 8, 10))


#--------------------------------------------------------
whr_panel %>%
  filter(
      !is.na(latam) # no missing values included
  ) %>%
  filter(year == 2015) %>%
  ggplot(aes(x = happiness_score, y = economy_gdp_per_capita,
             color = latam)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2", labels = c("No", "Yes")) +
  labs(
    x = "Happiness Score",
    y = "GDP per Capita",
    color = "Country in Latin America\nand the Caribbean",
    title = "Happiness Score vs GDP per Capita, 2015"
  ) +
  theme_minimal() 
