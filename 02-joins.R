library(tidyverse)
library(janitor)

encounters <- read_csv("data/synthea/encounters.csv") %>%
  clean_names()

encounters_mi <- encounters %>%
  filter(str_to_lower(description) %in%
           str_to_lower(c("cardiac arrest", "myocardial infarction")))

patients <- read_csv("data/synthea/patients.csv") %>%
  clean_names()

encounters_pt <- encounters_mi %>%
  left_join(patients, by = c("patient" = "id"))

nrow(encounters_pt)
nrow(encounters_mi)
nrow(patients)

stopifnot(nrow(encounters_pt) == nrow(encounters_mi))
stopifnot(nrow(encounters_pt) == 100)

library(lubridate)

# this doesn't work right now
#encounters_pt %>%
#  mutate(age_at_encounter = ymd_hms(start) - ymd(birthdate))
