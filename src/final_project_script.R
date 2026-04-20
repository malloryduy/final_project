# Download Packages 
library(tidyverse)
library(readr)
library(ggplot2)
library(janitor)
# Open CSV File 
COVID19_Outcomes_Tennessee <- read_csv("data/COVID19_Outcomes_Tennessee_by_Race_Sex.csv")
View(COVID19_Outcomes_Tennessee)
# Convert Date
COVID19_Outcomes_Tennessee <- COVID19_Outcomes_Tennessee |>
  mutate(
    WEEK_END_DATE = as.Date(WEEK_END_DATE, format = "%Y %b %d %I:%M:%S %p"))
#tidy data
COVID19_Outcomes_clean <- COVID19_Outcomes_Tennessee |>
  rename(
    group = CAT_DETAIL,
    date = WEEK_END_DATE,
    cases = CAT_TOTALCASES,
    deaths = CAT_TOTALDEATHS,
    hospitalizations = CAT_TOTALHOSP,
    category = CATEGORY
  )

COVID19_Outcomes_clean <- COVID19_Outcomes_clean |>
  pivot_longer(
    cols = c(cases, deaths, hospitalizations),
    names_to = "outcomes",
    values_to = "count"
  )
#Question's to ask 
###########################################
#Which racial group had the highest total COVID-19 cases per population?
###########################################

# create data table with race only 
race_data <- COVID19_Outcomes_clean |>
  filter(category == "RACE",outcomes == "cases" ) |>
  group_by(group) |> filter(date == max(date)) |>
  ungroup()
#open population table and tidy 
population_2022 <- read_csv("data/population_2022.csv")
population_2022 <- population_2022 |>  slice(c(39, 40, 41, 46, 54, 60)) |>
  select("Label (Grouping)", "Tennessee!!Estimate") |>
  rename( group = "Label (Grouping)", 
          population = "Tennessee!!Estimate") |>
  mutate(
    group = str_replace_all(group, fixed("\u00A0"), " "),
    group = str_squish(group),
    population = as.numeric(population)
  ) 
population_2022 <- population_2022  |>  mutate(
    group = case_when(
      group == "White" ~ "White",
      group == "Black or African American" ~ "Black or African American",
      group == "American Indian and Alaska Native" ~ "American Indian or Alaska Native",
      group == "Asian" ~ "Asian",
      group == "Native Hawaiian and Other Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
      group == "Two or More Races" ~ "Other/Multiracial"
    )
  ) 
race_joined <- race_data |> 
  left_join(population_2022, by = "group")
race_joined <- race_joined |>
  mutate(case_rate_per_100k = (count / population) * 100,000)
race_joined |> arrange(desc(case_rate_per_100k))
#testtt
###########################################
#How did COVID-19 case rates by race change over time? (temporal requirement)
###########################################

###########################################
#Which groups experiences disproportionately higher death rates relative to cases 
###########################################


race_cases_deaths <- COVID19_Outcomes_clean |>
  filter(category == "RACE", outcomes %in% c("cases", "deaths")) |>
  select(group, date, outcomes, count) |>
  pivot_wider(names_from = outcomes, values_from = count) |>
  mutate(case_fatality_rate = (deaths / cases) * 100)

race_cases_deaths_latest <- race_cases_deaths |>
  group_by(group) |>
  filter(date == max(date)) |>
  ungroup() |>
  arrange(desc(case_fatality_rate))

race_cases_deaths_latest

###########################################
#Which groups has the highest mortality rates after adjusting for population size? 
###########################################

race_death_rates <- COVID19_Outcomes_clean |>
  filter(category == "RACE", outcomes == "deaths") |>
  left_join(population_2022, by = "group") |>
  mutate(death_rate_per_100k = (count / population) * 100000)

race_death_rates_latest <- race_death_rates |>
  group_by(group) |>
  filter(date == max(date)) |>
  ungroup() |>
  arrange(desc(death_rate_per_100k))

race_death_rates_latest


