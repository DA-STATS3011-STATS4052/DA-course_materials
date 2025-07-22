## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(tidyverse)    # Data wrangling 
library(ggplot2)      # Data visualization
library(performance)  # Model assessment
library(skimr)        # Exploratory analysis
library(sjPlot)       # Plot and tables for linear models
library(broom)        # Linear model tidy summaries


## -----------------------------------------------------------------------------
evals <- read.csv("evals.csv",stringsAsFactors = T)
eval.score <- evals %>%
  dplyr::select(c(score,age,gender))





## ----co2, echo = TRUE, eval = TRUE, warning = FALSE---------------------------
eval.score %>%
  summarise(rho = cor(score,age))



## -----------------------------------------------------------------------------
eval.score %>%
  summarise(rho = cor(score,age),
            .by = gender)



## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-cap: "Instructor evaluation scores by age and gender. The points have been jittered."
#| fig-align: center
#| code-fold: show

ggplot(eval.score, 
       aes(x = age, y = score, color = gender)) +
  geom_jitter() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") 


## ----parmod, echo = TRUE, eval = TRUE, warning = FALSE------------------------
par.model <- lm(score ~ age + gender, data = eval.score)
tab_model(par.model,show.ci = F)


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 6
#| fig-height: 4
#| fig-cap: Instructor evaluation scores by age and gender with parallel regression lines superimposed.
plot_model(model = par.model,                 # The fitted model
           type="pred",               # type of plot
           terms = c("age","gender"), # terms to include in the plot
           grid=T,                    # split into a grid
           show.data = T,             # show observations
           jitter=T,                  # jitter the points
           ci.lvl = NA,               # show/hide confidence intervals
           title="Parallel Regression Model fitted lines") # Plot title


## -----------------------------------------------------------------------------
int.model <-lm(score ~ age * gender, data = eval.score)
tab_model(int.model,show.ci = F)


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 6
#| fig-height: 4
#| fig-cap: Instructor evaluation scores by age and gender with gender-varying regression lines superimposed.

plot_model(model = int.model,         # The fitted intertaction model
           type=  "pred",              # type of plot
           terms = c("age","gender"), # terms to include in the plot
           grid=F,                    # split into a grid
           show.data = T,             # show observations
           jitter=T,                  # jitter the points
           ci.lvl = NA,               # show/hide confidence intervals
           title ="Gender-age interaction model fitted lines", # Plot title
           colors = c("purple", "orange")) # color scheme


## -----------------------------------------------------------------------------
int.model_output <-  eval.score %>% 
  mutate(score_hat  = int.model$fitted.values,
         residual = int.model$residuals)


## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| fig-cap: "Residuals vs the explanatory variable age by gender."

ggplot(int.model_output, aes(x = age, y = residual)) +
  geom_point() +
  labs(x = "age", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", linewidth = 1) +
  facet_wrap(~ gender)


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-height: 4
#| fig-width: 8

check_model(int.model,check = c("linearity","homogeneity"))


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-height: 4
#| fig-width: 4
#| message: false
#| warning: false

int.model_diag <- plot_model(int.model,type = "diag")
int.model_diag[[4]]


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-height: 4
#| fig-width: 8

check_model(int.model,check = c("qq","normality"))


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-height: 4
#| fig-width: 8
#| message: false
#| warning: false

library(gridExtra) # to arrange the plots side-by-side
int.model_diag <- plot_model(int.model,type = "diag")

gridExtra::grid.arrange(int.model_diag[[2]], # qqplot
                        int.model_diag[[3]], # histogram
                        ncol=2) # plot them side by side




## ----echo = c(1,2)------------------------------------------------------------
slr.model <- lm(score ~ age, data = evals)

coeff <- slr.model %>%
          coef() 
coeff




## -----------------------------------------------------------------------------
tab_model(slr.model,show.se = T,show.ci = 0.95)


## -----------------------------------------------------------------------------
broom::tidy(slr.model,conf.int=T,conf.level = 0.95)


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4
#| fig-height: 4
#| fig-cap: "SLR model applied to the teaching evaluation Data."
plot_model(model = slr.model,
           terms = c("age"),
           type  = "pred",
           title = "Fitted Linear regression model",
           show.data = T,
           jitter = T)


## -----------------------------------------------------------------------------
#| webex.hide: "Click here to see the solution"
#| code-fold: show
#| fig-width: 4
#| fig-align: center
#| fig-height: 4
#| message: false
#| warning: true

ggplot(evals, aes(x = age, y = score)) +
  geom_jitter() +
  labs(x = "Age", y = "Teaching Score") +
 geom_smooth(method = "lm",level=.95)



## -----------------------------------------------------------------------------
tab_model(par.model,
          int.model,
          collapse.ci = T,
          show.stat = T,
          show.se = T,
          dv.labels = c("parallel lines model","interaction model") )


## -----------------------------------------------------------------------------
#| tbl-cap: "Estimates from the MLR model with `age` and `bty_avg`."
mlr.model <- lm(score ~ age + bty_avg, data = evals) 
tab_model(mlr.model)


## -----------------------------------------------------------------------------
#| echo: true
#| tbl-cap: Model comparisson for a multiple linear regression model agianst two nested simple linear regression models.
#| label: tbl-tab_comp
mlr.model <- lm(score ~ age + bty_avg, data = evals) 
slr.model_bty <- lm(score ~ bty_avg, data = evals) 
slr.model_age <- lm(score ~ age , data = evals) 

tab_model(mlr.model,slr.model_bty,slr.model_age,
          show.aic = T,
          collapse.ci = T)


## -----------------------------------------------------------------------------
broom::glance(mlr.model)
broom::glance(slr.model_bty)

