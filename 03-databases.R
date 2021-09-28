library(tidyverse)
library(RSQLite)

patients <- read_csv("data/synthea/patients.csv")

patients %>%
  filter(MARITAL == "M") %>%
  select(Id, BIRTHDATE, DEATHDATE, MARITAL) %>%
  head(5)

con <- dbConnect(SQLite(), "data/synthea/synthea.sqlite")

dbListTables(con)

patients_db <- tbl(con, "patients")

patients_db %>%
  filter(MARITAL == "M") %>%
  select(Id, BIRTHDATE, DEATHDATE, MARITAL) %>%
  head(5)

pt_db_5 <- dbGetQuery(con, 'SELECT * FROM patients LIMIT 5')
pt_db_5 <- dbGetQuery(con, 'SELECT Id, BIRTHDATE, DEATHDATE, MARITAL FROM patients LIMIT 5')
pt_db_5 <- dbGetQuery(con, 'SELECT Id, BIRTHDATE, DEATHDATE, MARITAL FROM patients WHERE MARITAL = "M" LIMIT 5')

patients_db %>%
  filter(MARITAL == "M") %>%
  select(Id, BIRTHDATE, DEATHDATE, MARITAL) %>%
  head(5) %>%
  show_query()
