## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| code-fold: show

library(tidyverse)
library(ggplot2)
library(sjPlot)
library(broom)
library(performance)
library(yardstick)



## -----------------------------------------------------------------------------
#| code-fold: false
#| 
turtles = read.csv("turtles.csv")
turtles%>% glimpse()


## -----------------------------------------------------------------------------
#| code-fold: false

turtles = turtles %>% 
  mutate(totals = male+female,
         male_props = male/totals)



## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-align: center
#| fig-height: 4
#| code-fold: show
ggplot(turtles,aes(y= male_props,x=temp))+
  geom_point()+ 
  labs(y="proportion of male hatchlings",x = "temperature")



## -----------------------------------------------------------------------------
#| eval: false
#| code-fold: false

# model_turtles <- glm(cbind(male,female) ~ temp,
#                      data = turtles,
#                      family = binomial)


## -----------------------------------------------------------------------------
#| code-fold: false

model_turtles <- glm(male_props ~ temp,
                     data = turtles,
                     weights =  totals,
                     family = binomial)


## -----------------------------------------------------------------------------
#| code-fold: false

model_turtles %>% tab_model(transform = NULL)






## -----------------------------------------------------------------------------
#| code-fold: show 
temp_pred = seq(27,30,by=0.01)
plot_model(model_turtles, 
           type = "eff", 
           title = "", 
           terms="temp[temp_pred]", 
           axis.title = c("Temperature", "Prob. of a hatchling being male"))



## -----------------------------------------------------------------------------
#| code-fold: false
check_overdispersion(model_turtles)


## -----------------------------------------------------------------------------
#| code-fold: show
#| 
turtles = turtles  %>% mutate(
  temp_fct = case_when(
    temp > 29 ~ "high",
    temp > 28 ~ "medium",
    .default = "low"
  ) %>% as.factor()
) 


## -----------------------------------------------------------------------------
#| code-fold: false
turtles = turtles %>%
  mutate(temp_fct = relevel(temp_fct,ref = "low")) 



## -----------------------------------------------------------------------------
#| code-fold: false
model_turtles_2 <- glm(cbind(male,female) ~ temp_fct,
                     data = turtles,
                     family = binomial)



## -----------------------------------------------------------------------------
#| code-fold: false

model_turtles_2 %>% tab_model(transform = "exp")




## -----------------------------------------------------------------------------
#| code-fold: false  
plogis(coef(model_turtles_2)[1])



## -----------------------------------------------------------------------------
#| code-fold: false  

plogis(coef(model_turtles_2)[1] + coef(model_turtles_2)[3])


## -----------------------------------------------------------------------------
#| code-fold: false  

plogis(coef(model_turtles_2)[1] + coef(model_turtles_2)[2])


## -----------------------------------------------------------------------------
#| code-fold: show
plot_model(type = "pred",
           model_turtles_2,
           terms = "temp_fct",
           axis.title = c("Temperature Category",
                          "Prob. of a hatchling being male"),
           title = " ")


## -----------------------------------------------------------------------------
#| warning: false
#| message: false
dcars = read.csv("Cars.csv",stringsAsFactors = T)

# Set "no/little" and "18-23"  as our reference categories

dcars = dcars %>%
  mutate(response = relevel(response,ref = "no/little"),
         age = relevel(age,ref="18-23")) 






## -----------------------------------------------------------------------------
#| code-fold: false  
#| results: hide

 ggplot(dcars, aes(x = age,
                   y = frequency, 
                   fill = response)) +
      geom_bar(stat = "identity",
               position = "dodge" )+
   labs(x = "Age groups",y ="Frequency")+
   scale_fill_manual(name = "Response category",
                     values = c("darkorange","purple","cyan4")) 
 
 ggplot(dcars, aes(x = sex,
                   y = frequency, 
                   fill = response)) +
      geom_bar(stat = "identity",
               position = "dodge" )+
   labs(x = "Sex",y ="Frequency")+
      scale_fill_manual(name = "Response category",
                        values = c("darkorange","purple","cyan4")) 



## -----------------------------------------------------------------------------
#| code-fold: false
#| message: false
#| warning: false

library(nnet)
model_cars <- multinom(response ~ age + sex, weight = frequency, data = dcars)


## -----------------------------------------------------------------------------
#| code-fold: false
model_cars %>% tab_model(transform = NULL)


## -----------------------------------------------------------------------------
#| code-fold: false
#| eval: false
# model_cars %>% tab_model(transform = "exp")


## -----------------------------------------------------------------------------
#| code-fold: false
model_null<- multinom(response ~ 1, data=dcars, weights=frequency)


## -----------------------------------------------------------------------------
#| code-fold: false
glance(model_null)
glance(model_cars)


## -----------------------------------------------------------------------------
#| code-fold: false

qchisq(df=6, p=0.95)
# pval
pchisq(77.844,df=6,lower.tail = F)


## -----------------------------------------------------------------------------
#| code-fold: false

anova(model_null,model_cars,test = "Chisq")



## -----------------------------------------------------------------------------
#| code-fold: false

model_full <- multinom(response ~ age * sex, weight = frequency, data = dcars)
glance(model_full)


## -----------------------------------------------------------------------------
#| code-fold: false

qchisq(df=4, p=0.95)
# pval
pchisq(3.94,df=4,lower.tail = F)


## -----------------------------------------------------------------------------
#| code-fold: false
anova(model_null,model_cars,model_full,test = "Chisq") 


## -----------------------------------------------------------------------------
#| code-fold: false
#| eval: false
#| results: hide

# library(performance)
# compare_performance(model_full,model_cars,model_null,metrics = "AIC")




## -----------------------------------------------------------------------------
#| code-fold: false

m.iris <- multinom(Species ~ Sepal.Length, data=iris)
cbind(iris,predict(m.iris)) %>% head()


