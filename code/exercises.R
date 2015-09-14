library(dplyr)
library(tidyr)
library(ggvis)
library(EDAWR)

pop <- population %>% gather("year", "population", 2:20, convert = TRUE)

tb3 <- tb %>% 
  mutate(cases = child + adult + elderly) %>%
  filter(!is.na(cases)) %>%
  select(country:sex, cases) %>%
  group_by(country, year) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  left_join(pop, by = c("country", "year")) %>%
  mutate(rate = cases / population * 10000)


tb3 %>% filter(country %in% c("Nepal", "Ghana", "Cambodia")) %>% ggvis(~population, ~cases, fill = ~country, size := 100)

china <- tb3 %>% filter(country == "China")

china %>% ggvis(x = ~year, y = ~rate) %>% layer_points()
china %>% ggvis(x = ~year, y = ~rate) %>% layer_lines()
china %>% ggvis(x = ~year, y = ~rate) %>% layer_bars()
china %>% ggvis(x = ~year, y = ~rate) %>% layer_smooths()

china %>% 
  ggvis(x = ~year, y = ~population, fill = ~year) %>%   
  layer_points()