library(tidyverse)

lcod <- read_csv("data/NCHS_-_Leading_Causes_of_Death__United_States.csv")

lcod

# Exercise
#
# 1. Filter the dataset to just look at the year 2017
# 2. Get the unique list of Causes from the `113 Cause Name` columns
# 3. Get the values for the `All Causes` death counts

lcod_2017 <- lcod %>%
  filter(Year == 2017)

lcod_2017 %>%
  distinct(`113 Cause Name`)

lcod_2017_all <- lcod_2017 %>%
  filter(`113 Cause Name` == "All Causes")

library(tidycensus)

# census_api_key("asldkflsadkfjklsd", install = TRUE)

acs5_2010_vars <- load_variables(2010, "acs5", cache = TRUE)

acs5_2010_vars <- read_csv("data/census/acs5_2010_vars.csv")

acs5_2010_vars %>%
  filter(str_detect(concept, "AGE")) %>%
  filter(concept == "SEX BY AGE")

census_dat <- get_acs(geography = "state", variables = "B01001_008", year = 2010)

census_dat %>%
  mutate(label = "Estimate!!Total!!Male!!20 years")

download_census <- function(var_name, label) {
  census_dat <- get_acs(geography = "state", variables = var_name, year = 2010)

  census_dat <- census_dat %>%
    mutate(label = label)
  return(census_dat)
}

download_census("B01001_004", "Estimate!!Total!!Male!!5 to 9 years")

library(purrr)

# census <- map2_df(acs5_2010_vars$name, acs5_2010_vars$label, download_census)
census <- read_csv("data/census/B01001-state-2010.csv")

census %>%
  distinct(label)


census_age <- census %>%
  filter(str_detect(label, "!!Male!!") | str_detect(label, "!!Female!!")) %>%
  separate(label, into = c("estimate_text", "total", "gender", "age_group"), sep="!!") %>%
  select(-estimate_text, -total)

pop_state <- census_age %>%
  group_by(NAME) %>%
  summarize(pop = sum(estimate))


cod_state_pop <- inner_join(lcod_2017_all, pop_state, by = c("State" = "NAME")) %>%
  mutate(death_adj_rate_pop = (Deaths / pop) * 100000)
