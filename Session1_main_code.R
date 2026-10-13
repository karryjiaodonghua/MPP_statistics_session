rm(list = ls())

a <-c (1:5)
a<-a+5

getwd()

# set the path of the datasheet that you will use

install.packages("here")
install.packages("dplyr")
library(here)
library(dplyr)

path <- here("DataWork","Datasets", "Final","whr_panel.csv")
whr <- read.csv(path)

#--------------------------------------------
#create a list
my_list <- list(
  name = "Anna",
  age = 20,
  scores = c(80, 90, 100)
)

#return a sub-list
my_list["scores"] 

#return the vector (extract the content of the sub-list)
my_list[["scores"]]
#it is the same as using $
my_list$scores

#---------------------------------------------
#how functions work
result <- mean(whr$happiness_score, na.rm = TRUE)
mean(whr$happiness_score, na.rm = TRUE)
#--------------------------------------------
#factor: categorical data:
income_group <- factor(
  c("Low", "High", "Middle", "Low"),
  levels = c("Low", "Middle", "High"),
  ordered = TRUE
)

#it can be used for comparison, normally you can't compare two lists of characters. 
income_group > "Middle"



#---------------------------------------------
students <- data.frame(
  name = c("Anna", "Ben", "Chen", "Diana"),
  age = c(20, 22, 21, 24),
  score = c(75, 88, 92, 65),
  country = c("UK", "US", "China", "UK")
)

head(whr)       # first 6 rows
tail(whr)       # last 6 rows
dim(whr)        # rows, columns
nrow(whr)       # number of rows
ncol(whr)       # number of columns
names(whr)      # column names

str(whr)        # compact structure
class(whr)      # object class
summary(whr)    # summaries by column
View(whr)       # spreadsheet-style viewer

quantile(whr$happiness_score)
mean(whr$economy_gdp_per_capita, na.rm=TRUE)
max(whr$happiness_score)
min(whr$happiness_score)
sd(whr$happiness_score)
var(whr$happiness_score)

#--------------------------------------------
# Intermediate object: easier to inspect and debug
log_score <- log(whr$happiness_score)
summary(log_score)

# Nested functions: compact, but can become hard to read
summary(log(whr$happiness_score))

#--------------------------------------------
library(dplyr)

whr2016 <- whr %>%
  filter(year == 2016) %>%                         # keep rows
  select(country, region, happiness_score) %>%    # keep/reorder columns
  mutate(score_100 = happiness_score * 10) %>%    # create/change columns
  arrange(desc(happiness_score))                  # reorder rows

regional_summary <- whr %>%
  group_by(region) %>%
  summarise(
    mean_happiness = mean(happiness_score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )