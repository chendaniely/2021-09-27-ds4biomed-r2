library(tidyverse)

encounters <- read_csv("data/synthea/encounters.csv")

encounters %>%
  group_by(DESCRIPTION) %>%
  summarise(count = n()) %>%
  arrange(-count)

encounters %>%
  group_by(REASONDESCRIPTION) %>%
  summarise(count = n()) %>%
  arrange(-count)

encounters %>%
  group_by(DESCRIPTION, REASONDESCRIPTION) %>%
  summarise(count = n()) %>%
  arrange(-count)

names(encounters)

library(janitor)

encounters <- clean_names(encounters)
names(encounters)

encounters %>%
  select(id, start, stop, patient, encounterclass, description, reasondescription)

encounters_col_sub <- encounters %>%
  select(id:patient, encounterclass, description, reasondescription)

encounters_col_sub %>%
  filter(description == "Cardiac Arrest" | description == "Myocardial Infarction")

mi_terms <- c("Cardiac Arrest", "Myocardial Infarction") %>%
  str_to_lower()

mi_terms

mi_encounters <- encounters_col_sub %>%
  filter(str_to_lower(description) %in% mi_terms)

pt_mi_repeat_ids <- mi_encounters %>%
  count(patient) %>%
  arrange(-n) %>%
  filter(n > 1) %>%
  pull(patient)

pt_encounter_mi <- encounters_col_sub %>%
  filter(patient %in% pt_mi_repeat_ids) %>%
  arrange(patient, start, stop)

pt_encounter_mi %>%
  distinct(encounterclass)

py_encounter_mi_emergency <- pt_encounter_mi %>%
  filter(encounterclass %in% c("emergency", "urgentcare"))

mi_terms

pt_encounter_mi_only <- pt_encounter_mi %>%
  filter(str_to_lower(description) %in% mi_terms)

pt_encounter_mi_only

library(lubridate)

ymd("2020-04-28")

pt_encounter_mi_only <- pt_encounter_mi_only %>%
  mutate(stop_dt = ymd_hms(stop),
         start_dt = ymd_hms(start)) %>%
  select(-start, -stop) %>%
  mutate(encounter_length = stop_dt - start_dt)

pt_encounter_mi_only %>%
  mutate(next_encounter_start = lead(start_dt),
         next_encounter_length = next_encounter_start - start_dt
         )

mi_reencounter <- pt_encounter_mi_only %>%
  group_by(patient) %>%
  mutate(next_encounter_start = lead(start_dt),
         next_encounter_length = next_encounter_start - start_dt
  ) %>%
  ungroup()

mi_reencounter %>%
  filter(next_encounter_length < 30)

mi_reencounter %>%
  filter(next_encounter_length < 356.25 * 5)

mi_reencounter %>%
  filter(next_encounter_length < 356.25 * 10)
