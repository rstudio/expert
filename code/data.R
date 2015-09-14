# Making data sets for Expert Data Analysis with R
library(dplyr)
library(tidyr)

# gather example
# Source: World Health Organization Global Tuberculosis Report
# http://www.who.int/tb/country/data/download/en/
cases <- data.frame(stringsAsFactors = FALSE,
  country = c("FR", "DE", "US"),
  y2011 = c(7000, 5800, 15000),
  y2012 = c(6900, 6000, 14000),
  y2013 = c(7000, 6200, 13000))
names(cases) <- c("country", "2011", "2012", "2013")
write.csv(cases, file = "data-raw/cases.csv", row.names = FALSE)
save(cases, file = "data/cases.rdata")

# spread example
# Source: Ambient Air Pollution Database, WHO, May 2014.
# http://www.who.int/phe/health_topics/outdoorair/databases/cities/en/
pollution <- data.frame(stringsAsFactors = FALSE,
  city = rep(c("New York", "London", "Beijing"), each = 2),
  type = rep(c("PM_10", "PM_2.5"), 3),
  value = c(23, 14, 22, 16, 121, 56))
write.csv(pollution, file = "data-raw/pollution.csv", row.names = FALSE)
save(pollution, file = "data/pollution.rdata")

# tidy example
library(nasaweather)
library(lubridate)
storms <- storms %>% 
  unite("date", year, month, day, sep = "-") %>%
  group_by(name) %>% 
  summarise(wind = max(wind), pressure = pressure[which.max(wind)], 
            date = as.Date(date[which.max(wind)])) %>% 
  head(6)
write.csv(storms, file = "data-raw/storms.csv", row.names = FALSE)
save(storms, file = "data/storms.rdata")


songs <- data.frame(stringsAsFactors = FALSE,
  song = c("Across the Universe", "Come Together", "Hello, Goodbye", "Peggy Sue"), 
  name = c("John", "John", "Paul", "Buddy"))
write.csv(songs, file = "data-raw/songs.csv", row.names = FALSE)
save(songs, file = "data/songs.rdata")

songs2 <- data.frame(stringsAsFactors = FALSE,
  song = c("Across the Universe", "Come Together", "Hello, Goodbye", "Peggy Sue"), 
  first = c("John", "John", "Paul", "Buddy"),
  last = c("Lennon", "Lennon", "McCartney", "Holly") )
write.csv(songs2, file = "data-raw/songs2.csv", row.names = FALSE)
save(songs2, file = "data/songs2.rdata")

artists <- data.frame(stringsAsFactors = FALSE,
  name = c("George", "John", "Paul", "Ringo"), 
  plays = c("sitar", "guitar", "bass",  "drums"))
write.csv(artists, file = "data-raw/artists.csv", row.names = FALSE)
save(artists, file = "data/artists.rdata")

artists2 <- data.frame(stringsAsFactors = FALSE,
  first = c("George", "John", "Paul", "Ringo", "Paul", "John"), 
  last = c("Harrison", "Lennon", "McCartney",  "Starr", "Simon", "Coltrane"),
  plays = c("sitar", "guitar", "bass",  "drums", "guitar", "sax"))
write.csv(artists2, file = "data-raw/artists2.csv", row.names = FALSE)
save(artists2, file = "data/artists2.rdata")

beatles <- data.frame(stringsAsFactors = FALSE,
  name = c("George", "John", "Paul", "Ringo"), 
  last = c("Harrison", "Lennon", "McCartney",  "Starr"))

y <- data.frame(x1 = c("A", "B", "C"), x2 = 1:3, stringsAsFactors = FALSE)
write.csv(y, file = "data-raw/y.csv", row.names = FALSE)
save(y, file = "data/y.rdata")

z <- data.frame(x1 = c("B", "C", "D"), x2 = 2:4, stringsAsFactors = FALSE)
write.csv(z, file = "data-raw/z.csv", row.names = FALSE)
save(z, file = "data/z.rdata")

a <- data.frame(x1 = c("A", "B", "C"), x2 = 1:3, stringsAsFactors = FALSE)
write.csv(a, file = "data-raw/a.csv", row.names = FALSE)
save(a, file = "data/a.rdata")

b <- data.frame(x1 = c("A", "B", "D"), x2 = c(TRUE, FALSE, TRUE), stringsAsFactors = FALSE)
write.csv(b, file = "data-raw/b.csv", row.names = FALSE)
save(b, file = "data/b.rdata")

toyb <- data.frame(stringsAsFactors = FALSE,
  country = rep(c("Afghanistan", "Brazil", "China"), each = 4),
  year = rep(rep(c(1999, 2000), each = 2), 3),
  sex = rep(c("female", "male"), 6), 
  cases = rep(c(1:3), each = 4))
write.csv(toyb, file = "data-raw/toyb.csv", row.names = FALSE)
save(toyb, file = "data/toyb.rdata")

a <- c(NA, 2, 3, NA)
b <- c(1, NA, 3, NA)
c <- c(1, 2, NA, NA)
nas <- data_frame(a, b, c)
write.csv(nas, file = "data-raw/nas.csv", row.names = FALSE)
save(nas, file = "data/nas.rdata")

tidypop <- population %>% gather("year", "population", 2:20, convert = TRUE)
write.csv(tidypop, file = "data-raw/tidypop.csv", row.names = FALSE)
save(tidypop, file = "data/tidypop.rdata")


who2 <-
  who %>%
  gather("key", "cases", 5:60) %>%
  separate(key, c("new", "type", "sexage")) %>%
  separate(sexage, c("sex", "age"), sep = 1) %>%
  spread(type, cases)

who3 <-
  who2 %>%
  filter(!(is.na(ep) & is.na(rel) & is.na(sn) & is.na(sp))) %>%
  mutate(cases = plus(ep, rel, sn, sp)) %>%
  select(country, year, sex, age, cases) %>%
  arrange(country, year, sex, age)

who4 <-
  who3 %>%
  group_by(country, year) %>%
  summarise(cases = sum(cases)) %>%
  ungroup()

rates <-
  who4 %>%
  left_join(tidypop) %>%
  mutate(rate = round(cases / population * 10000, 2))

write.csv(rates, file = "data-raw/rates.csv", row.names = FALSE)
save(rates, file = "data/rates.rdata")