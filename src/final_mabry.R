#load and inspect data
library(tidyverse)
outcomes <- read_csv("data/COVID19_Outcomes_Tennessee_by_Race_Sex.csv")
glimpse(outcomes)
colnames(outcomes)


# clean column names
outcomes <- outcomes %>%
  rename(
    detail = CAT_DETAIL,
    week_end_date = WEEK_END_DATE,
    category = CATEGORY,
    cases = CAT_TOTALCASES,
    deaths = CAT_TOTALDEATHS,
    hosp = CAT_TOTALHOSP
  )


#race over time
race_data <- outcomes |>
  filter(group_type == "RACE")

race_summary <- race_data |>
  group_by(group, week_end_date) |>
  summarise(
    cases = max(cases),
    deaths = max(deaths),
    hosp = max(hosp),
    .groups = "drop"
  )

#need to fix the x-axis
ggplot(race_summary, aes(x = week_end_date, y = cases, color = group, group = group)) +
  geom_line() +
  theme_minimal()

#who has the highest case burden
latest_cases <- race_summary |>
  group_by(group) |>
  filter(week_end_date == max(week_end_date)) |>
  arrange(desc(cases))

latest_cases

ggplot(latest_cases, aes(x = reorder(group, cases), y = cases)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total COVID-19 Cases by Race",
    x = "Race",
    y = "Total Cases"
  ) +
  theme_minimal()