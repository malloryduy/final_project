# Download Packages 
library(tidyverse)
library(readr)
library(ggplot2)
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
  filter(category == "RACE",outcomes == "cases" ) |> filter(date == max(date))
#open population table and tidy 
population_2022 <- read_csv("data/population_2022.csv")
population_2022 <- population_2022 |>  slice(c(39, 40, 41, 46, 54, 60)) |>
  select("Label (Grouping)", "Tennessee!!Estimate") |>
  rename( group = "Label (Grouping)", 
          population = "Tennessee!!Estimate") |>
  mutate(
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
  left_join(population_2022, by = "group") |> 
  mutate(case_rate_per_100k = (count / population) * 100000) |>
  arrange(desc(case_rate_per_100k))

###########################################
#How did COVID-19 case rates by race change over time? (temporal requirement)
###########################################

#Filter the dataset to focus only on race and case data
race_data <- COVID19_Outcomes_clean |>
  filter(category == "RACE", outcomes == "cases")

# Summarize cases by race and date
# Why: We combine repeated entries so each race has one total case value per week
race_summary <- race_data |>
  group_by(group, date) |>
  summarise(
    cases = sum(count, na.rm = TRUE),
    .groups = "drop"
  )

#Step 3: Create a time trend line graph
ggplot(race_summary, aes(x = date, y = cases, color = group, group = group)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "COVID-19 Case Trends Over Time by Race in Tennessee",
    x = "Date",
    y = "Cases",
    color = "Race"
  )

###########################################
#Which groups experiences disproportionately higher death rates relative to cases 
###########################################

#For the first question, I filtered the data to only race, cases, and deaths. Then I reshaped the data so cases and deaths were in separate columns. After that, I calculated the case fatality rate by dividing deaths by cases and multiplying by 100. Then I kept the most recent date for each group and sorted from highest to lowest.

#STEP 1: start with the cleaned dataset
#STEP 2: (line 2) keep only the race data and only the outcomes I need
#Step 3 (line 3): keep only the important columns. I kept:
#group for the racial group
#date for the week
#outcomes to show whether the number is cases or deaths
#count for the actual value

#step 4 (line 4): reshape the data so cases and deaths are in seperate columns. Originally the cases and deaths were stacked in one column under outcomes but this step changes the data so each row has one column for cases and one column for deaths. Important because I need both values in the same row to calculate the death rate relative to cases. 

#Step 5 (line 5): creates a new column called case_datality_rate. This formula is deaths divided by cases then multiplied by 100. This gives the percent of COVID cases that resulted in death for each racial group. 


race_cases_deaths <- COVID19_Outcomes_clean |>
  filter(category == "RACE", outcomes %in% c("cases", "deaths")) |>
  select(group, date, outcomes, count) |>
  pivot_wider(names_from = outcomes, values_from = count) |>
  mutate(case_fatality_rate = (deaths / cases) * 100)

#Step 6: keep only the most recent data for each group. I grouped the data by racial group, then kept only the most recent date for each one. I did this so I would be comparing the latest available values instead of older weeks.

#Step 7: (ungroup) : Remove the grouping. This just returns the dataset to normal so the next step works cleanly. 

#Step 8 (arrange): Sort from highest to lowest, This puts the racial group with the highest case fatality rate at the top. 


race_cases_deaths_latest <- race_cases_deaths |>
  group_by(group) |>
  filter(date == max(date)) |>
  ungroup() |>
  arrange(desc(case_fatality_rate))


#Step 9: print final table. This lets me see which groups had highest death rates relative to cases. 

race_cases_deaths_latest

###########################################
#Which groups has the highest mortality rates after adjusting for population size? 
###########################################

#For the second question, I filtered the data to only race and deaths, then joined it with population data. After that, I calculated the death rate per 100,000 people by dividing deaths by population and multiplying by 100,000. Then I kept the most recent date for each racial group and sorted from highest to lowest.

#Step 1: Start with the cleaned COVID dataset again. I used COVID19_Outcomes_clean because it already contains the COVID data in a format I can filter easily.

#Step 2:(filter) Keep only race data and only deaths. For this question, I do not need cases or hospitalizations. I only need deaths because I am calculating mortality rates.

#Step 3: (left_join): This joins the COVID death data with my population dataset using the race group name. After this step, each row has:
#the racial group
#the date
#the number of deaths
#the population for that group
#This is necessary because I cannot adjust for population size unless population is included.

#Step 4: (mutate): This creates a new column called death_rate_per_100k.
#The formula is:
  #deaths divided by population
#then multiplied by 100,000
#I multiply by 100,000 because that is a standard way to compare rates across groups of different sizes.

race_death_rates <- COVID19_Outcomes_clean |>
  filter(category == "RACE", outcomes == "deaths") |>
  left_join(population_2022, by = "group") |>
  mutate(death_rate_per_100k = (count / population) * 100000)


#Step 5: keep only the most recent data for each group. Again, I grouped by racial group and kept only the latest date so I would be comparing the most current data.

#Step 6: (ungroup): remove grouping. This resets the data frame back to normal.

#Step 7: Sort from highest to lowest. This places the group with the highest mortality rate per 100,000 at the top. 

#Step 8: print final table. This shows which racial groups had the highest mortality rates after accoutning for pop. size. 

race_death_rates_latest <- race_death_rates |>
  group_by(group) |>
  filter(date == max(date)) |>
  ungroup() |>
  arrange(desc(death_rate_per_100k))

race_death_rates_latest




