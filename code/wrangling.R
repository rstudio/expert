# EDAWR

# install.packages(c("tidyr", "dplyr", "ggvis", "devtools"))
# devtools::install_github("rstudio/EDAWR")
library(EDAWR)
?tb
?population

View(tb)
View(population)

library(dplyr)

select(storms2, storm, pressure)
select(storms2, -storm)
filter(storms, wind >= 50)
filter(storms, wind >= 50,
  storm %in% c("Alberto", "Alex", "Allison"))

?Comparison
?base::Logic

filter(tb, !is.na(child), !is.na(adult), !is.na(elderly))
select(tb, child:elderly)

tb %>% 
  filter(!is.na(child), !is.na(adult), !is.na(elderly)) %>%
  select(child:elderly)

select(tb, child:elderly)
tb %>% select(child:elderly)

tb2 <- filter(tb, !is.na(child))
tb3 <- select(tb2, child:elderly)
tb4 <- mutate(tb3, diff = adult - child)

tb %>% 
  filter(!is.na(child)) %>%
  select(child:elderly) %>%
  mutate(diff = adult - child)

mutate(storms, ratio = pressure / wind)
mutate(storms, ratio = pressure / wind, inverse = ratio^-1)
storms %>%
  mutate(ratio = pressure / wind) %>%
  select(storm, ratio)

tb %>% 
  mutate(cases = child + adult + elderly) %>%
  filter(!is.na(cases)) %>%
  select(country:sex, cases)

pollution %>% summarise(median = median(amount), variance = var(amount))
pollution %>% summarise(mean = mean(amount), sum = sum(amount), n = n())

pollution %>% group_by(city)

pollution %>% group_by(city) %>% 
  summarise(mean = mean(amount), sum = sum(amount), n = n())

pollution %>% group_by(city) %>%  summarise(mean = mean(amount))
pollution %>% group_by(size) %>%  summarise(mean = mean(amount))
pollution %>% group_by(city, size) %>%  summarise(mean = mean(amount))

tb %>% 
  mutate(cases = child + adult + elderly) %>%
  filter(!is.na(cases)) %>%
  select(country:sex, cases) %>%
  group_by(country, year) %>%
  summarise(cases = sum(cases))

rawtb
rawtb %>% group_by(country, year, sex, age) %>% summarise(n = sum(n))

