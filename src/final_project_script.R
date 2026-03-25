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
# Make simple plot to see all cases overtime 
#Aggregate Data 
COVID_outcomes_summary <- COVID19_Outcomes_Tennessee |>
  group_by(WEEK_END_DATE) |>
  summarise(total_cases = sum(CAT_TOTALCASES, na.rm = TRUE))
View(COVID_outcomes_summary)
#Make plot
ggplot(COVID_outcomes_summary, aes(x = WEEK_END_DATE, y= total_cases)) +
  geom_line() + labs(
    title = "COVID19 Cases Over Time in Tennessee", 
    x= "Date",
    y = "Total Cases")