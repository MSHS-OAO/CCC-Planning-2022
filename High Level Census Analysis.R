## Load packages ------------
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(svDialogs)
library(stringr)
library(formattable)
library(scales)
library(ggpubr)
library(reshape2)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(zipcodeR)
library(tidyr)
library(janitor)

## Clear history ------------
rm(list = ls())

## Determine path for working directory --------------
if ("Presidents" %in% list.files("J://")) {
  user_directory <- paste0("J:/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "System Operations/CCC Strike Planning")
} else {
  user_directory <- paste0("J:/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "System Operations/CCC Strike Planning")
}

## Import data ---------
### Census data
raw_df <- read_excel(path = paste0(user_directory,
                                   "/Data/Command Center Raw Data for Kate Revised with ADT IP Patient 10.21.22.xlsx"))

### Dept mapping
dept_mapping <- read_excel(path = paste0(user_directory,
                                         "/Data",
                                         "/Census Dept Mappings 2022-10-18-arz-v2.xlsx"))

dept_mapping_crosswalk <- dept_mapping %>%
  select(-Notes, -`ARZ Notes`)

census_date_start <- as.Date("3/1/21", format = "%m/%d/%y")
census_date_end <- as.Date("5/31/21", format = "%m/%d/%y")

dates_df <- data.frame("CensusDate" = seq.Date(from = census_date_start,
                                               to = census_date_end,
                                               by = "day"))

dates_df <- dates_df %>%
  mutate(DOW = wday(CensusDate, label = TRUE, abbr = TRUE),
         WkendWkday = ifelse(DOW %in% c("Sat", "Sun"), "Weekend",
                             "Weekday"))

total_days <- nrow(dates_df)

dow_summary <- dates_df %>%
  group_by(DOW) %>%
  summarize(Count = n()) %>%
  ungroup()

wkend_wkday_summary <- dates_df %>%
  group_by(WkendWkday) %>%
  summarize(Count = n()) %>%
  ungroup()

unit_type_order <- c("ICU",
                     "Med Surg",
                     "Tele",
                     "ED",
                     "L&D",
                     "Mother Baby",
                     "NICU",
                     "Peds",
                     "Peds ICU",
                     "Rehab",
                     "Psych",
                     "Psych ED")

site_order <- c("MSH", "MSM", "MSW")

census_range_df <- raw_df %>%
  rename(Encounter = `External Enc Id`,
         AdmitDate = `Admit Date`,
         DischDate = `Dsch Date`,
         CensusDate = `Census Date`,
         CensusDept = `Census Dept`,
         DischSite = `Ip Site`) %>%
  mutate(AdmitDate = as.Date(AdmitDate),
         DischDate = as.Date(DischDate),
         CensusDate = as.Date(CensusDate),
         CensusDay = ifelse(CensusDate == DischDate, 0, 1),
         DOW = wday(CensusDate, label = TRUE, abbr = TRUE),
         DOWNum = wday(CensusDate, label = FALSE),
         WkendWkday = ifelse(DOW %in% c("Sat", "Sun"), "Weekend", 
                             "Weekday")) %>%
  filter(between(CensusDate, census_date_start, census_date_end))

census_range_df <- left_join(census_range_df,
                             dept_mapping_crosswalk,
                             by = c("CensusDept" = "Dept"))

census_range_df <- census_range_df %>%
  rename(CensusSite = Site,
         UnitType = `Unit Type`) %>%
  filter(Include %in% "Yes") %>%
  mutate(UnitType = factor(UnitType,
                           levels = unit_type_order,
                           ordered = TRUE),
         CensusSite = factor(CensusSite,
                             levels = site_order,
                             ordered = TRUE)) %>%
  arrange(CensusSite, UnitType, DOWNum)


## ADC across all days -----------------
unit_type_total_adc <- census_range_df %>%
  group_by(CensusSite,
           UnitType) %>%
  summarize(CensusDays = sum(CensusDay, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ADC = CensusDays / total_days)

unit_type_total_adc_table <- unit_type_total_adc %>%
  select(-CensusDays) %>%
  pivot_wider(names_from = CensusSite,
              values_from = ADC) %>%
  adorn_totals(where = "row",
               fill = "-",
               na.rm = TRUE)

## ADC by weekend/weekdays ----------------------
unit_type_wkendwkday_adc <- census_range_df %>%
  group_by(CensusSite,
           UnitType,
           WkendWkday) %>%
  summarize(CensusDays = sum(CensusDay, na.rm = TRUE)) %>%
  ungroup()

unit_type_wkendwkday_adc <- left_join(unit_type_wkendwkday_adc,
                                      wkend_wkday_summary,
                                      by = c("WkendWkday" = "WkendWkday"))

unit_type_wkendwkday_adc <- unit_type_wkendwkday_adc %>%
  mutate(ADC = CensusDays / Count)

unit_type_wkendwkday_adc_table <- unit_type_wkendwkday_adc %>%
  select(-CensusDays, -Count) %>%
  pivot_wider(names_from = c(CensusSite, WkendWkday),
              names_sep = "_",
              values_from = ADC) %>%
  adorn_totals(where = "row",
               fill = "-",
               na.rm = TRUE)

## ADC by day of week ---------------------
unit_type_dow_adc <- census_range_df %>%
  group_by(CensusSite,
           UnitType,
           DOW) %>%
  summarize(CensusDays = sum(CensusDay, na.rm = TRUE)) %>%
  ungroup()

unit_type_dow_adc <- left_join(unit_type_dow_adc,
                               dow_summary,
                               by = c("DOW" = "DOW"))  

unit_type_dow_adc <- unit_type_dow_adc %>%
  mutate(ADC = CensusDays / Count)

unit_type_dow_adc_table <- unit_type_dow_adc %>%
  select(-CensusDays, -Count) %>%
  pivot_wider(names_from = c(CensusSite, DOW),
              names_sep = "_",
              values_from = ADC) %>%
  adorn_totals(where = "row",
               fill = "-",
               na.rm = TRUE)


