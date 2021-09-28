library(tidyverse)

tb <- read_csv("data/tb_long.csv")
tb

tb_tidy <- tb %>%
  pivot_longer(starts_with(c("m", "f"))) %>%
  separate(name, into = c("sex", "age_group"), sep = 1)
tb_tidy

tb_tidy %>%
  distinct(age_group)

clean_age_group <- function(age_group_string) {
  if (age_group_string == "u") {
    return("Unknown")
  } else if (str_starts(age_group_string, "0")) {
    age_cat <- paste0(str_sub(age_group_string, start = 1, end = 1), "-", str_sub(age_group_string, start = 2, end = -1))
    return(age_cat)
  } else if (age_group_string == "65") {
    return("65+")
  } else if (str_length(age_group_string) == 4) {
    age_cat <- paste0(str_sub(age_group_string, start = 1, end = 2), "-", str_sub(age_group_string, start = 3, end = -1))
    return(age_cat)
  }
}

library(glue)

clean_age_group <- function(age_group_string) {
  if (age_group_string == "u") {
    return("Unknown")
  } else if (str_starts(age_group_string, "0")) {
    p1 <- str_sub(age_group_string, start = 1, end = 1)
    p2 <- str_sub(age_group_string, start = 2, end = -1)
    age_cat <- paste0(p1, "-", p2)
    return(age_cat)
  } else if (age_group_string == "65") {
    return("65+")
  } else if (str_length(age_group_string) == 4) {
    p1 <- str_sub(age_group_string, start = 1, end = 2)
    p2 <- str_sub(age_group_string, start = 3, end = -1)
    age_cat <- glue("{p1}-{p2}")
    return(age_cat)
  }
}

clean_age_group("u")
clean_age_group("014")

str_starts("014", "0")

str_sub("014", start = 1, end = 1)
str_sub("014", start = 2, end = -1)

stopifnot(clean_age_group("u") == "Unknown")
clean_age_group("014")
stopifnot(clean_age_group("014") == "0-14")
clean_age_group("65")
stopifnot(clean_age_group("65") == "65+")
str_length("1534")
stopifnot(clean_age_group("3645") == "36-45")
stopifnot(clean_age_group("4554") == "45-54")

tb_tidy

tb_tidy %>%
  clean_age_group(age_group)

tb_tidy %>%
  mutate(age_cat = clean_age_group(age_group))

library(purrr)

map_chr(tb_tidy$age_group, clean_age_group)

tb_tidy <- tb_tidy %>%
  mutate(age_cat = map_chr(age_group, clean_age_group))
tb_tidy

library(ggplot2)

year_age_count <- tb_tidy %>%
  group_by(year, age_cat) %>%
  summarize(sum = sum(value, na.rm=TRUE))

ggplot(year_age_count, aes(x = age_cat, y = sum)) +
  geom_bar(stat="identity") +
  facet_wrap(~ year)
