# Session 4: More on Regression and Graph




library(tidyverse)
library(dplyr)
library(here)
library(janitor)
library(modelsummary)
library(fixest)
library(huxtable)
library(plotly)
library(report)
library(visreg)


# ----------------------------------------------
WHR2016 <- whr_panel %>% filter(year == 2016) %>%
  select(country, region, happiness_score, economy_gdp_per_capita,
         health_life_expectancy, freedom) %>% drop_na()




m1 <- lm(happiness_score ~ economy_gdp_per_capita, data=WHR2016)
m2 <- lm(happiness_score ~ economy_gdp_per_capita +
           health_life_expectancy + freedom, data=WHR2016)

models <- list("gdp/capita only"=m1, "with controls"=m2)


# --------------------------------------------------
# lm(y ~ .)


reg_data <- WHR2016 %>%
  select(happiness_score, economy_gdp_per_capita,
         health_life_expectancy, freedom)
m_all <- lm(happiness_score ~ ., data=reg_data)
coef(m_all)


# --------------------------------------------------
# Interactions: lm(y ~ x1 * x2)

# x1 * x2 includes both main effects and their product; the slope can vary with x2.
m_interact <- lm(happiness_score ~ economy_gdp_per_capita * freedom +
              health_life_expectancy, data=WHR2016)
coef(m_interact)


# --------------------------------------------------
# Interactions: lm(y ~ x1:x2)

m_product <- lm(happiness_score ~ 
                  economy_gdp_per_capita:freedom,
                data=WHR2016)
m_interact <- lm(happiness_score ~ 
                  economy_gdp_per_capita * freedom,
                data=WHR2016)
modelsummary(list("Product only"=m_product,
                  "Main effects + product"=m_interact),
             output="kableExtra")


# --------------------------------------------------
# visreg()

install.packages("visreg")

visreg(m2, "economy_gdp_per_capita",
               gg=TRUE, partial=FALSE,rug = FALSE) +
  labs(x="GDP per capita", y="Happiness Score Est")

report(m2)


# --------------------------------------------------
# Polynomial regression
m_quad <- lm(happiness_score ~ economy_gdp_per_capita +
               I(economy_gdp_per_capita^2), data=WHR2016)

m_poly <- lm(happiness_score ~
               poly(economy_gdp_per_capita, 2,raw=TRUE), data=WHR2016)
anova(m1, m_quad)

# --------------------------------------------------
# Plot linear and quadratic fits

ggplot(WHR2016, aes(economy_gdp_per_capita, happiness_score)) +
  geom_point(alpha=.5) +
  geom_smooth(aes(color="Linear"), method="lm",
              formula=y ~ x) +
  geom_smooth(aes(color="Quadratic"), method="lm",
              formula=y ~ x + I(x^2)) +
  labs(x="GDP contribution score", y="Happiness", color="Fit")


# --------------------------------------------------
# Fixed Effect - feols()

census <- census %>% select(state, region, divorce, pop, popurban, marriage) %>%
  drop_na()
# lm() can also add fixed effect by using factor()
reg1 <- lm(divorce ~ pop + popurban + marriage + factor(region), data=census)
reg2 <- feols(divorce ~ pop + popurban + marriage | region,
              data=census, 
              vcov= ~state # this defines clustered std errors by state
             )
summary(reg2)


# --------------------------------------------------
# Pooled OLS and two-way fixed effects

panel <- whr_panel %>%
  select(country, year, happiness_score, economy_gdp_per_capita,
         health_life_expectancy, freedom) %>% drop_na()
m_pool <- feols(happiness_score ~ economy_gdp_per_capita +
                  health_life_expectancy + freedom,
                data=panel, vcov=~country)
m_fe <- feols(happiness_score ~ economy_gdp_per_capita +
                health_life_expectancy + freedom | country + year,
              data=panel, vcov=~country)


# --------------------------------------------------
# Instrumental Variables

reg_iv <- feols(
  divorce ~ pop | region | marriage ~ popurban,
  data = census,
  vcov = "hetero")

summary(reg_iv, stage=1)
summary(reg_iv)

huxreg(reg1, reg2)

census_table <- huxreg(
  "OLS"=reg1, "Region FE"=reg2,
  coefs=c(
    "Constant"="(Intercept)",
    "Population"="pop", 
    "Urban population"="popurban",
    "Marriages"="marriage",
    "North Central"="factor(region)N Cntrl",
    "South"="factor(region)South",
    "West"="factor(region)West"),
  statistics=c("N"="nobs", "R-squared"="r.squared"), 
  number_format=3,
  stars=c("***"=.01, "**"=.05, "*"=.1),
  note=paste("Region FE: Yes / Yes. SEs: conventional / state-clustered.",
             "{stars} Standard errors in parentheses."))
census_table

# --------------------------------------------------
reg1_feols <- feols(
  divorce ~ pop + popurban + marriage + factor(region),
  data = census
)

# Region entered through the fixed-effects section
reg2 <- feols(
  divorce ~ pop + popurban + marriage | region,
  data = census
)

etable(reg1_feols, reg2,
       dict=c("(Intercept)"="Constant",
            "pop"="Population", 
            "popurban"="Urban population",
            "marriage"="Marriages",
            "factor(region)N Cntrl"="North Central",
            "factor(region)South"="South",
            "factor(region)West"="West")
       )


# --------------------------------------------------
quick_xlsx(census_table, file=here("census_model.xlsx"))
                     
quick_latex(census_table, file="output/census_models.tex")


# --------------------------------------------------
modelsummary(models, output="kableExtra",
  coef_map=coef_labels, 
  vcov="HC1", conf_level=.95,
  fmt=3, stars=TRUE,
  statistic=c("SE = {std.error}", "p = {p.value}","[{conf.low}, {conf.high}]"),
  gof_map=c("nobs", "r.squared", "adj.r.squared"),
  title="Happiness associations, 2016",
  notes="95% intervals using HC1 robust standard errors.",
  escape = FALSE)


# --------------------------------------------------
library(stargazer)
stargazer(m1, m2, type="text",
  dep.var.labels="Happiness score",
  covariate.labels=c("GDP/capita", "Health", "Freedom"),
  omit.stat=c("f", "ser"))
# File outputs:
stargazer(m1, m2, type="html", out=here("models.html"))
stargazer(m1, m2, type="latex", out=here("models.tex"))


# --------------------------------------------------
p <- ggplot(WHR2016, 
      aes(economy_gdp_per_capita, happiness_score)) +
      geom_point(color="#287C8E", alpha=.7) +
      labs(title=
             "Happiness and the GDP contribution score",
        subtitle=
        "Explore the relationship between GDP/capita and happiness score",
        x="GDP/capita", y="Happiness score",
        caption="Source: WHR dataset.") +
        theme_minimal(base_size=13)
print(p)


# --------------------------------------------------
ggsave(filename=here("happiness_gdp.png"), plot=p,
       width=7, height=4.5, units="in", dpi=300, bg="white")
ggsave(filename=here("happiness_gdp.pdf"), plot=p,
       width=7, height=4.5, units="in")


# --------------------------------------------------
#geom_histogram()
ggplot(WHR2016, aes(happiness_score)) +
  geom_histogram(binwidth=.5, boundary=0,
                 fill="#287C8E", color="white") +
  labs(x="Happiness score", y="Number of countries")


# --------------------------------------------------
#geom_boxplot()
ggplot(WHR2016, 
       aes(
         x=reorder(region, happiness_score, median),
         y=happiness_score)) +
  geom_boxplot(fill="#D38C28", outlier.shape=NA) +
  geom_jitter(width=.12, height=0, alpha=.35, size=1) +
  coord_flip() + 
  labs(x=NULL, y="Happiness score")+
  theme_classic()


# --------------------------------------------------
#geom_line()
line_data <- whr_panel %>%
  filter(country %in% 
        c("China", "United States", "United Kingdom")) %>%
  arrange(country, year)

ggplot(line_data, aes(x=year, 
                      y=happiness_score,
                      color=country,
                      group=country)) +
  geom_line(linewidth=.9) +
  scale_x_continuous(breaks=2015:2017) +
  labs(x="Year", y="Happiness score", 
       color="Country")


# --------------------------------------------------
#geom_line()+geom_point()
ggplot(line_data, aes(x=year, 
                   y=happiness_score, 
                   color=country,
                  group=country)) +
  geom_line(linewidth=.8) + 
  geom_point(size=2.5) +
  scale_x_continuous(breaks=2015:2017) +
  labs(x=NULL, y="Happiness score", color=NULL) +
  theme_minimal() +
  theme(legend.position="bottom",
         legend.text=element_text(size=8))


# --------------------------------------------------
# ggplotly()
interactive_p <- ggplot(WHR2016,
  aes(economy_gdp_per_capita, happiness_score,
      text=paste("Country:", country))) +
  geom_point(aes(color=region)) +
  labs(x="GDP contribution score", y="Happiness")
  ggplotly(interactive_p, 
                 tooltip=c("text", "x", "y"),) %>%
    layout(showlegend = FALSE)


# --------------------------------------------------
plot_ly(WHR2016,
  x=~economy_gdp_per_capita, 
  y=~health_life_expectancy,
  z=~happiness_score, text=~country,
  type="scatter3d", mode="markers", width=NULL, height=400,
  marker=list(size=3, opacity=.7)) %>%
  
  layout(scene=list(xaxis=list(title="GDP score"),
    yaxis=list(title="Health score"),
    zaxis=list(title="Happiness")))


