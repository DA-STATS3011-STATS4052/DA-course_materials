## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| code-fold: show

library(ggplot2)
library(tidyverse)
library(nycflights13)
library(fivethirtyeight)


## -----------------------------------------------------------------------------
#| label: "View the Data"
glimpse(flights)


















## ----readcsv, message=FALSE, eval=TRUE, echo = 1------------------------------
dem_score <- read_csv("https://moderndive.com/data/dem_score.csv")


## -----------------------------------------------------------------------------
#| code-fold: show
guat_dem <-  dplyr::filter(dem_score,country == "Guatemala")
guat_dem


## -----------------------------------------------------------------------------
#| echo: true
#| code-fold: show
guat_dem_long = pivot_longer(guat_dem,cols = !country,
                             names_to = "year",
                             values_to = "democracy_score")
slice(guat_dem_long,1:5)


## -----------------------------------------------------------------------------
#| code-fold: false

guat_dem_wide = pivot_wider(guat_dem_long,
                            names_from = year, 
                            values_from = democracy_score)
guat_dem_wide


## -----------------------------------------------------------------------------
#| eval: false
#| code-fold: false
#| label: "Task"

# library(fivethirtyeight)
# drinks






## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 3
#| fig-align: center
#| code-fold: show
#| label: "Scatterplot Democracy vs. year"

ggplot(data = guat_dem_long, mapping =aes(x = year, y = democracy_score)) +
  geom_point()










## -----------------------------------------------------------------------------
#| code-fold: show
#| eval: false
#| label: "Select variables"

# flights %>%
#   select(carrier, flight)




## -----------------------------------------------------------------------------
#| code-fold: show
#| label: "Remove variables"
flights_no_year <- flights  %>% select(-year)


## -----------------------------------------------------------------------------
#| code-fold: show
#| label: "Select variables with helper functions"

flights_reorder <- flights %>%
  select(month:day, hour:time_hour, everything())
names(flights_reorder)




## -----------------------------------------------------------------------------
#| code-fold: show
#| label: "rename variables"

flights_time <- flights %>%
  select(contains("time")) %>%
  rename(departure_time = dep_time, arrival_time = arr_time)
names(flights_time)




## -----------------------------------------------------------------------------
#| message: false
#| echo: false
library(conflicted)


## -----------------------------------------------------------------------------
#| code-fold: show
conflict_prefer("filter", "dplyr")


## -----------------------------------------------------------------------------
#| warning: false
#| message: false
#| fig-width: 5
#| fig-height: 4
#| fig-align: center
#| code-fold: show
#| label: "Scatterplot Arrival vs Departue delays"

flights %>%
  filter(carrier ==  "AS") %>%
  ggplot(aes(x = dep_delay, y = arr_delay)) +
  geom_point()+
   labs(x = "Departure delay (minutes)", y = "Arrival delay (minutes)",
       title = "Alaska Airlines flights leaving NYC in 2013")


## -----------------------------------------------------------------------------
#| code-fold: show
#| label: "Filter variables"
btv_sea_flights_fall <- flights %>%
  filter(origin == "JFK", (dest == "BTV" | dest == "SEA"), month >= 10) %>%
  relocate(dest,.before = dep_time )




## -----------------------------------------------------------------------------
#| code-fold: show
#| label: "Filter variables 2"
not_BTV_SEA <- flights %>%
  filter(!(dest == "BTV" | dest == "SEA")) %>%
  relocate(dest,.before = dep_time )
not_BTV_SEA %>%
  slice(1:3)






## -----------------------------------------------------------------------------
#| code-fold: show
#| label: "Mutate verb"
flights <- flights %>%
  mutate(gain = dep_delay - arr_delay)




## -----------------------------------------------------------------------------
#| message: false
#| fig-width: 4
#| fig-height: 3
#| fig-align: center
#| warning: false
#| code-fold: show
#| label: "Histogram of gained time"


ggplot(data = flights, mapping = aes(x = gain)) +
  geom_histogram(color = "white", bins = 20)


## -----------------------------------------------------------------------------
#| code-fold: show
#| include: false
#| label: "Create multiple columns"
flights <- flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )
flights %>% select(c(gain,hours,gain_per_hour))








## -----------------------------------------------------------------------------
#| code-fold: show
#| label: "Summarise one variable"
summary_temp <- weather %>%
  summarize(mean = mean(temp, na.rm = TRUE), std_dev = sd(temp, na.rm = TRUE))
summary_temp


## -----------------------------------------------------------------------------
#| echo: false
opts_p <- c(
  answer = "Introduces a selection bias since patient who died due to lung cancer are excluded from the analysis, leading to an underestimation of the true impact of smoking on lung cancer risk",
    "There is no problem, smaller datasets with fewer missing values may require less computational resources, leading to faster processing times.",
  answer = "Removing patients with missing data reduces the sample size. Hence, conclusions may not be as easily generalizable to the broader population, as the excluded patients may represent a different subset with unique characteristics.", "Removing missing values can result in a dataset with fewer errors and inconsistencies, which can lead to more accurate analyses."
)




## ----eval=FALSE, echo = c(1,2,3,4)--------------------------------------------
#| code-fold: show
#| label: "Group-level summaries"
# summary_monthly_temp <- weather %>%
#   summarize(mean = mean(temp, na.rm = TRUE),
#             std_dev = sd(temp, na.rm = TRUE),
#             .by = month)




## -----------------------------------------------------------------------------
#| code-fold: show
#| eval: false
#| label: "Summarise counts"
# by_origin <- flights %>%
#   summarize(count = n(),
#             .by =origin)
# by_origin






## -----------------------------------------------------------------------------
#| code-fold: show
#| label: "Summarise by multiple groups"
by_origin_monthly <- flights %>%
  summarize(count = n(),
            .by = c(origin, month))
by_origin_monthly


## -----------------------------------------------------------------------------
#| fig-width: 5
#| fig-align: center
#| fig-height: 4
#| code-fold: false
#| label: "Barplot for unaggregated count data"
flights %>%
  ggplot(aes(x=factor(month),fill=origin))+
  geom_bar()+
  scale_x_discrete(labels = month.abb) +
  labs(x= "Months",y="Number of flights")


## -----------------------------------------------------------------------------
#| fig-width: 5
#| fig-align: center
#| fig-height: 4
#| code-fold: false
#| label: "Barplot for aggregated count data"

by_origin_monthly %>%
ggplot(aes(x = factor(month), y = count, fill= origin )) +
  geom_col() +
  scale_x_discrete(labels = month.abb)+
    labs(x= "Months",y="Number of flights")






## -----------------------------------------------------------------------------
#| code-fold: show
#| eval: false
#| label: "if else case when statements"
# weather %>%
#   mutate(
#     temp_cat = case_when(
#       is.na(temp) ~ NA,
#       temp < 39.9 ~ "low",
#       between(temp,39.9 ,70)~ "medium",
#       .default = "large"
#     )
#   ) %>%
#   relocate(temp,temp_cat)
# 






## -----------------------------------------------------------------------------
#| eval: false
#| code-fold: false
#| label: "Airlines data set"
# airlines






## -----------------------------------------------------------------------------
#| code-fold: show 
#| label: "Join data frame by a common key with same names"
flights_joined <- flights %>%
  inner_join(airlines,
             by = join_by(carrier))




## -----------------------------------------------------------------------------
#| eval: false
#| code-fold: false
#| label: "Airports data set"
# airports




## -----------------------------------------------------------------------------
#| code-fold: show
#| eval: false
#| label: "Join data frame by a common key with different names"

# flights %>%
#   inner_join(airports,
#              by = join_by(dest == faa))


## -----------------------------------------------------------------------------
#| code-fold: show
#| eval: false
#| label: "Large pipeline command"
# named_dests <- flights %>%
#   summarize(num_flights = n(),
#             .by = dest)  %>%
#   arrange(desc(num_flights))  %>%
#   inner_join(airports, by = join_by(dest == faa)) %>%
#   rename(airport_name = name)
# named_dests




## -----------------------------------------------------------------------------
#| code-fold: show
#| label: "Join data sets with multiple keys"
flights_weather_joined <- flights  %>%
  inner_join(weather,
             by = join_by(year,month,day,hour,origin))

flights_weather_joined

