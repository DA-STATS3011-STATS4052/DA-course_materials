## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| code-fold: show

library(tidyverse)
library(ggplot2)
library(sjPlot)
library(broom)
library(performance)
library(yardstick)  # model validation


## -----------------------------------------------------------------------------
#| code-fold: show
evals <- read.csv("evals.csv",stringsAsFactors = T)
evals.gender <- evals %>%
                  select(gender, age)


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| fig-cap: "Teaching instructor age by gender."

ggplot(data = evals.gender, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Age") +
  theme(legend.position = "none")





## -----------------------------------------------------------------------------
#| code-fold: false

model <- glm(gender ~ age, data = evals.gender,family = binomial)




## -----------------------------------------------------------------------------
#| code-fold: false
model %>% tab_model(show.ci = F,transform = NULL)



## -----------------------------------------------------------------------------
#| code-fold: false
#| message: false
model %>% broom::tidy()




## -----------------------------------------------------------------------------
#| code-fold: false
levels(evals.gender$gender)




## -----------------------------------------------------------------------------
#| code-fold: false
#| tbl-cap: "Logistic regression with a continuous covariate log-odds scale estimates "
#| label: tbl-summaries_logOdds_conti
model %>% tab_model(transform = NULL,show.ci = 0.95)


## -----------------------------------------------------------------------------
#| code-fold: false

model %>% broom::tidy(conf.int = TRUE, conf.level = 0.95)





## -----------------------------------------------------------------------------
#| code-fold: show
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| fig-cap: "The log-odds of age for male instructors." 
plot_model(model, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Male instructor)", show.p = FALSE)


## -----------------------------------------------------------------------------
#| webex.hide: "See the solution"
#| code-fold: false
#| echo: true

mod.coef.logodds <- model %>% tidy() %>% filter(term=="age")

# Compute Wald 95% CI by hand
mod.coef.logodds$estimate + (c(-1,1)*
qnorm(0.975)) * mod.coef.logodds$std.error 

# Compare with Wald 95% CI in R
confint.default(model)[2,]


## -----------------------------------------------------------------------------
#| code-fold: false
model %>% tab_model(transform = "exp",show.ci = NULL)


## -----------------------------------------------------------------------------
#| code-fold: false
model %>% broom::tidy(exponentiate = T)




## -----------------------------------------------------------------------------
#| code-fold: false
#| eval: true
#| tbl-cap: "Logistic regression with a continuous covariate odd-scale estimates "
#| label: tbl-summaries_Odds_conti
model %>% tab_model(transform = "exp",show.ci = 0.95) 


## -----------------------------------------------------------------------------
#| code-fold: false
#| eval: false

# model %>% broom::tidy(exponentiate = T,conf.int = T)




## -----------------------------------------------------------------------------
#| code-fold: show
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| fig-cap: "The odds of age for male instructors." 
#| 
plot_model(model, show.values = TRUE,
           title = "Odds (Male instructor)", show.p = FALSE, axis.lim = c(1, 1.5))


## -----------------------------------------------------------------------------
#| code-fold: false
plogis(model$coefficients[1] 
              + model$coefficients[2] *  52)


## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| fig-cap: "Probability of teaching instructor being male by age."
#| warning: false
#| message: false
#| code-fold: show
plot_model(model, 
           type = "pred", 
           title = "", 
           terms="age [all]", 
           axis.title = c("Age", "Prob. of instructor being male"))



## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| code-fold: show
#| fig-align: center
#| fig-width: 18
#| fig-height: 10
#| fig-cap: "GLM diagnostics for the gender of teaching instructors"

library(performance)
check_model(model, check = c("pp_check","binned_residuals","outliers","qq"))


## -----------------------------------------------------------------------------
#| code-fold: show

pred_results = model %>% 
  broom::augment(type.predict = c("response")) %>%
  mutate(predicted_class = 
           factor(ifelse(.fitted > 0.5, "male", "female")))





## -----------------------------------------------------------------------------
#| code-fold: false
#| message: false
#| warning: false
library(yardstick)
conf_mat(pred_results,truth = gender,estimate = predicted_class)


## -----------------------------------------------------------------------------
#| code-fold: false
#| eval: false

# # Step (1) create a set with classification metrics we want to compute
# eval_metric <- metric_set(accuracy,sensitivity,specificity,ppv,npv)
# # Step (2) call out the metric set and input the data containing the observed and predicted classes
# eval_metric(pred_results,
#             truth = gender,
#             estimate = predicted_class,
#             event_level = "second")












## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| fig-cap: "ROC curve for GLM fitted to the evaluation scores data set."
#| code-fold: false

roc_curve(pred_results,
          truth = gender,
          .fitted,
          event_level = "second") %>%
  autoplot()



## -----------------------------------------------------------------------------
#| code-fold: false
#| eval: false

# yardstick::roc_auc(pred_results,
#         truth = gender,
#         .fitted,
#         event_level = "second")






## -----------------------------------------------------------------------------
#| code-fold: show
evals.ethnic <- evals %>%
                  select(gender, ethnicity)







## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4
#| fig-height: 4
#| fig-cap: "Barplot of teaching instructors' gender by ethnicity."
#| echo: false
ggplot(evals.ethnic, aes(x = gender, group = ethnicity)) +
    geom_bar(aes(y = after_stat(prop), fill = ethnicity), 
             stat = "count", position = "dodge") +
    labs(y = "Proportion", fill = "Ethnicity")




## -----------------------------------------------------------------------------
#| code-fold: false
model.ethnic <- glm(gender ~ ethnicity,
                    data = evals.ethnic,
                    family = binomial) 



## -----------------------------------------------------------------------------
#| code-fold: false 
levels(evals.ethnic$ethnicity)


## -----------------------------------------------------------------------------
#| code-fold: false
#| tbl-cap: "Logistic regression with a categorical covariate log-odds scale estimates "
#| label: tbl-summaries_logOdds_categorical
model.ethnic %>%
  tab_model(transform = NULL)




## -----------------------------------------------------------------------------
#| code-fold: show
#| fig-height: 4
#| fig-width: 4
#| fig-align: center
#| fig-cap: "The log-odds for male instructors by ethnicity (not a minority)."

plot_model(model.ethnic, 
           show.values = TRUE, 
           transform = NULL, 
           title = "Log-Odds (Male instructor)", 
           show.p = FALSE)


## -----------------------------------------------------------------------------
#| code-fold: false 
#| tbl-cap: "Logistic regression with a categorical covariate odd-scale estimates "
#| label: tbl-summaries_Odds_categorical
model.ethnic %>%
  tab_model(transform = "exp")








## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| fig-cap: "The odds-ratio of a male instructor given they are in the `not minority` group."
#| code-fold: show
plot_model(model.ethnic,
           transform = "exp",
           show.values = TRUE,
           title = "Odds (Male instructor)",
           show.p = FALSE)


## -----------------------------------------------------------------------------
#| code-fold: false
plogis(coef(model.ethnic)[1])
plogis(coef(model.ethnic)[1]+coef(model.ethnic)[2])




## -----------------------------------------------------------------------------
#| fig-height: 4
#| fig-width: 4
#| fig-align: center
#| fig-cap: "Probability of teaching instructor being male by ethnicity."
#| code-fold: show
plot_model(model.ethnic,
           type = "pred",
           terms = "ethnicity",
           axis.title = c("Ethnicity", 
                          "Prob. of instructor being male"),
           title = " ")

