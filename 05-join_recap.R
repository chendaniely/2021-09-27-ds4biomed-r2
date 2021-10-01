library(tidyverse)
library(janitor)

encounters <- read_csv("./data/synthea/encounters.csv") %>%
  clean_names()
