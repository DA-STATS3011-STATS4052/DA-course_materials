## ----libraries2, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE----
library(tidyverse)    # Data wrangling 
library(ggplot2)      # Data visualization
library(performance)  # Model assessment
library(skimr)        # Exploratory analysis
library(sjPlot)       # Plot and tables for linear models






## -----------------------------------------------------------------------------
#| echo: false
evals <- read.csv("evals.csv")


## ----evals, echo = TRUE, eval = TRUE, warning = FALSE-------------------------
evals.scores <- evals %>%
  dplyr::select(score, bty_avg)


## ----evals3, echo = TRUE, eval = TRUE, warning = FALSE------------------------
evals.scores %>%
  skim()






## ----correlation3, echo = TRUE, eval = TRUE, warning = FALSE------------------

evals.scores %>% summarise(rho = cor(score,bty_avg))



## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| fig-cap: "Relationship between teaching and beauty scores."
ggplot(evals.scores, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score", y = "Teaching Score", title = "Relationship of teaching and beauty scores")




## ----lm, echo = TRUE, eval = TRUE, warning = FALSE----------------------------
model <- lm(score ~ bty_avg, data = evals.scores)
tab_model(model,show.ci = F)




## ----lm2, echo = TRUE, eval = TRUE,message=FALSE, warning = FALSE, fig.cap = "Relationship between teaching and beauty scores with regression line superimposed.", out.width = '50%'----
ggplot(evals.scores, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score", y = "Teaching Score", 
       title = "Relationship of teaching and beauty scores") +
  geom_smooth(method = "lm", se = FALSE)




## -----------------------------------------------------------------------------
 model_output <- evals.scores %>% 
   mutate(score_hat  = model$fitted.values,
          residuals  = model$residuals)
 model_output %>% slice(1:6)


## -----------------------------------------------------------------------------
#| code-fold: show
#| fig-cap: "Residuals against beauty score."
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
ggplot(model_output, aes(x = bty_avg, y = residuals)) +
  geom_point() +
  labs(x = "Beauty Score", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", linewidth = 1)


## -----------------------------------------------------------------------------
#| code-fold: show
#| fig-cap: "Residuals against fitted values."
#| fig-width: 7
#| fig-height: 4
#| fig-align: center

check_model(model,check = c("homogeneity","linearity"))



## ----lm9----------------------------------------------------------------------
#| code-fold: show
#| fig-cap: "QQ-plot and Histogram of residuals."
#| fig-width: 8
#| fig-height: 4
#| fig-align: center
check_model(model,check = c("normality","qq"))



## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-align: center
#| fig-height: 4
#| fig-width: 4

diag_plots = plot_model(model,type='diag')

# QQ plot
diag_plots[[1]]
# Histrogram/density plot
diag_plots[[2]]
# Residuals vs fitted values
diag_plots[[3]]




## -----------------------------------------------------------------------------
gapminder <- read.csv("gapminder.csv")

gapminder2007 <- gapminder %>%
  dplyr::filter(year == 2007) %>% 
  dplyr::select(country, continent, lifeExp) %>%
  mutate(country  = as.factor(country),
         continent = as.factor(continent))


## ----gap2, echo = TRUE, eval = TRUE, warning = FALSE--------------------------
glimpse(gapminder2007)


## ----gap3, echo = TRUE, eval = TRUE, warning = FALSE--------------------------
gapminder2007 %>% 
  select(continent, lifeExp) %>% 
  skim()


## -----------------------------------------------------------------------------
lifeExp.continent <- gapminder2007 %>%
  summarize(median = median(lifeExp), mean = mean(lifeExp),.by = continent)
lifeExp.continent


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4
#| fig-height: 4
#| fig-cap: "Life expectancy by continent in 2007."

ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy (years)", 
       title = "Life expectancy by continent")






## -----------------------------------------------------------------------------
lifeExp.model <- lm(lifeExp ~ continent, data = gapminder2007)
tab_model(lifeExp.model,show.ci = F)


## -----------------------------------------------------------------------------
gapminder2007 %>% slice(1:8)


## -----------------------------------------------------------------------------
#| eval: false
# lifeExp.model_output <- gapminder2007 %>%
#   mutate(lifeExp_hat  = lifeExp.model$fitted.values,
#          residual = lifeExp.model$residuals)
# 
# lifeExp.model_output %>% slice(1:10)




## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-align: center
#| fig-height: 4
#| fig-cap:  "Residuals over continent."
#| code-fold: show

ggplot(lifeExp.model_output, aes(x = continent, y = residual)) +
  geom_jitter(width = 0.1) + 
  labs(x = "Continent", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue")


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-align: center
#| fig-height: 4
#| fig-cap:  "Residuals vs fitted values."
#| code-fold: show
check_model(lifeExp.model,check=c("homogeneity","linearity"))




## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-align: center
#| fig-height: 4
#| fig-cap:  "QQ-plot and Histogram of residuals."
#| code-fold: show
check_model(lifeExp.model,check=c("qq","normality"))


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| eval: false
#| echo: false

# library(ISLR)
# data("Credit")
# write.csv(Credit,file = "Credit.csv",row.names = F)


## -----------------------------------------------------------------------------
Credit = read.csv("Credit.csv")




## ----skim, echo = TRUE, eval = TRUE, warning = FALSE--------------------------
Cred %>%
  skim()


## ----cor, echo = TRUE, eval = TRUE, warning = FALSE---------------------------
Cred %>%
  cor()




## -----------------------------------------------------------------------------
#| fig-height: 4
#| fig-width: 4
#| fig-align: center
#| fig-cap: "Relationship between balance and credit limit."
#| code-fold: show

ggplot(Cred, aes(x = Limit, y = Balance)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Credit card balance (in $)", 
       title = "Relationship between balance and credit limit") 


## -----------------------------------------------------------------------------
#| fig-height: 4
#| fig-width: 4
#| fig-align: center
#| fig-cap: "Relationship between balance and income."
#| code-fold: show


ggplot(Cred, aes(x = Income, y = Balance)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Credit card balance (in $)", 
       title = "Relationship between balance and income") 


## -----------------------------------------------------------------------------
#| fig-width: 5
#| fig-height: 5
#| fig-align: center
#| fig-cap: "3D scatterplot of balance, credit limit, and income."
#| code-fold: show
#| message: false
#| warning: false
library(plotly)
plot_ly(Cred, x = ~Income, y = ~Limit, z = ~Balance,
        type = "scatter3d", mode = "markers")


## ----mod, echo = TRUE, eval = TRUE, warning = FALSE---------------------------
Balance.model <- lm(Balance ~ Limit + Income, data = Cred)
tab_model(Balance.model,show.ci = F)


## -----------------------------------------------------------------------------
Balance.model_output <-  Cred %>% 
  mutate(Balance_hat  = Balance.model$fitted.values,
         residual = Balance.model$residuals)
  


## -----------------------------------------------------------------------------
#| fig-height: 4
#| fig-width: 4
#| fig-align: center
#| fig-cap:  "Residuals vs credit limit."
#| code-fold: show

ggplot(Balance.model_output, aes(x = Limit, y = residual)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Residual", title = "Residuals vs credit limit")  +
  geom_hline(yintercept = 0, col = "blue", linewidth = 1)


## -----------------------------------------------------------------------------
#| fig-height: 4
#| fig-width: 4
#| fig-align: center
#| fig-cap:  "Residuals vs income."
#| code-fold: show
ggplot(Balance.model_output, aes(x = Income, y = residual)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Residual", title = "Residuals vs income") +
  geom_hline(yintercept = 0, col = "blue", linewidth = 1)


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-height: 8
#| fig-width: 8
#| fig-align: center
#| fig-cap:  "Balance.model Residuals checks."
#| code-fold: show
check_model(Balance.model, check= c("linearity","homogeneity","qq","normality"))



## -----------------------------------------------------------------------------
#| webex.hide: "Click here to see the solution"
#| code-fold: show
#| fig-width: 4
#| fig-align: center
#| fig-height: 4

diag_plots = plot_model(Balance.model,type='diag')

# QQ plot
diag_plots[[2]]
# Histrogram/density plot
diag_plots[[3]]
# Residuals vs fitted values
diag_plots[[4]]



## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 6
#| fig-height: 4
check_model(Balance.model,check="vif")


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4
#| fig-height: 4
diag_plots[[1]]

