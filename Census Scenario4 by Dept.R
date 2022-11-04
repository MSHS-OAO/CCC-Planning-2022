
# Code for creating summary of MSH, MSM, and MSW census analysis -----------------
#Install and load necessary packages --------------------
# install.packages("readxl")
# install.packages("writexl")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("svDialogs")
# install.packages("stringr")
# install.packages("formattable")
# install.packages("kableExtra")
# install.packages("ggpubr")
# install.packages("zipcodeR")
# install.packages("tidyr")
#Analysis for weekend discharge tracking
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
library(purrr)
library(janitor)
library(DT)

# Clear history
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
                                         "/Census Dept and Admit Mappings 2022-10-25.xlsx"),
                           sheet = "Census Dept")

dept_mapping_crosswalk <- dept_mapping %>%
  select(-Notes, -`ARZ Notes`)

admit_type_mapping <- read_excel(path = paste0(user_directory,
                                               "/Data",
                                               "/Census Dept and Admit Mappings 2022-10-25.xlsx"),
                                 sheet = "Admit Type")

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
                     "ED IP Boarder",
                     "L&D",
                     "Mother Baby",
                     "Mother Baby - Nursery",
                     "NICU",
                     "Peds",
                     "Peds ICU",
                     "Rehab",
                     "Psych",
                     "Psych ED")

site_order <- c("MSH", "MSM", "MSW")

wkday_wkend_order <- c("Weekday", "Weekend")

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

census_range_df <- left_join(census_range_df,
                             admit_type_mapping,
                             by = c("Admission Type" = "AdmissionType"))

census_range_df <- census_range_df %>%
  rename(CensusSite = Site,
         UnitType = `Unit Type`) %>%
  filter(Include %in% "Yes") %>%
  mutate(UnitType = factor(UnitType,
                           levels = unit_type_order,
                           ordered = TRUE),
         CensusSite = factor(CensusSite,
                             levels = site_order,
                             ordered = TRUE),
         WkendWkday = factor(WkendWkday,
                             levels = wkday_wkend_order,
                             ordered = TRUE)) %>%
  arrange(CensusSite, UnitType, DOWNum)

dept_summary_epic <- census_range_df %>%
  filter(CensusSite %in% c("MSM", "MSW")) %>%
  group_by(CensusSite, CensusDept) %>%
  summarize(Census = sum(CensusDay, na.rm = TRUE))

## ADC by site and department -----------------

unit_type_total_adc <- census_range_df %>%
  group_by(CensusSite,
           UnitType,
           CensusDept) %>%
  summarize(CensusDays = sum(CensusDay, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ADC = CensusDays / total_days) %>%
  select(-CensusDays)


## ADC by weekend/weekdays ----------------------
unit_type_wkendwkday_adc <- census_range_df %>%
  group_by(CensusSite,
           WkendWkday,
           UnitType,
           CensusDept) %>%
  summarize(CensusDays = sum(CensusDay, na.rm = TRUE)) %>%
  ungroup()

unit_type_wkendwkday_adc <- left_join(unit_type_wkendwkday_adc,
                                      wkend_wkday_summary,
                                      by = c("WkendWkday" = "WkendWkday"))

unit_type_wkendwkday_adc <- unit_type_wkendwkday_adc %>%
  mutate(ADC = CensusDays / Count) %>%
  select(-CensusDays, -Count)

## ADC Census Scenario 4: 5% Elective Admissions -------------------
scenarios <- c(.05)
names(scenarios) <- paste0("Scenario",
                           # which(scenarios == scenarios),
                           "_",
                           100 * scenarios,
                           "Percent_Elective")

scenarios_df <- as.data.frame(t(scenarios))

unit_type_admit_type_adc <- census_range_df %>%
  group_by(CensusSite,
           WkendWkday,
           UnitType,
           CensusDept,
           ElectiveNonElective) %>%
  summarize(CensusDays = sum(CensusDay, na.rm = TRUE)) %>%
  ungroup()

unit_type_admit_type_adc <- left_join(unit_type_admit_type_adc,
                                      wkend_wkday_summary,
                                      by = c("WkendWkday" = "WkendWkday"))

unit_type_admit_type_adc <- unit_type_admit_type_adc %>%
  pivot_wider(names_from = ElectiveNonElective,
              values_from = CensusDays) %>%
  rowwise() %>%
  mutate(Total = sum(Elective, `Non-Elective`, na.rm = TRUE),
         Elective_ADC = Elective / Count,
         NonElective_ADC = `Non-Elective` / Count, 
         Total_ADC = Total / Count,
         across(where(is.numeric), ~replace_na(.x, 0)),
         Elective_Percent = percent(Elective / Total),
         NonElective_Percent = percent(`Non-Elective` / Total)) %>%
  relocate(Elective_ADC, .after = NonElective_ADC) %>%
  relocate(Elective_Percent, .after = NonElective_Percent)

census_scenarios <- cbind(unit_type_admit_type_adc,
                          scenarios_df)

census_scenarios <- census_scenarios %>%
  mutate(across(contains("Scenario"),
                function(x) {
                  x * .$Elective_ADC
                },
                .names = "{.col}_ADC"
  )) %>%
  # rowwise() %>%
  mutate(across(matches("^(Scenario).*(ADC)$"),
                function(x) {
                  replace_na(x, 0) + .$NonElective_ADC
                },
                .names = "{.col}_Total")
  ) %>%
  # select(-matches("^(Scenario).*(Elective)$")) %>%
  rename_with(.fn = ~ str_replace(.x, "Elective_ADC_Total", "Total_ADC"),
              .cols = matches("^(Scenario).*(_Total)$")) %>%
  select(-Count,
         -Elective, -`Non-Elective`, -Total) %>%
  select(colnames(.[!str_detect(colnames(.), "^(Scenario).*(ADC)")]),
         sort(colnames(.[str_detect(colnames(.), "^(Scenario).*(ADC)$")]))) %>%
  relocate(WkendWkday, .after = CensusSite) %>%
  arrange(CensusSite, WkendWkday, UnitType)

## Format department mappings ------------
excluded_unit_types <- unique(dept_mapping_crosswalk$`Unit Type`)

excluded_unit_types <- excluded_unit_types[!(excluded_unit_types %in% unit_type_order)]

all_unit_types <- c(unit_type_order, excluded_unit_types)

dept_mapping_table <- dept_mapping_crosswalk %>%
  mutate(Site = factor(Site, levels = site_order, ordered = TRUE),
         `Unit Type` = factor(`Unit Type`, levels = all_unit_types,
                              
                              ordered = TRUE)) %>%
  relocate(`Unit Type`, .after = Site) %>%
  arrange(Site, `Unit Type`, desc(Include))

## Export data to Excel ---------------

export_list <- list("ADC Summary" = unit_type_total_adc,
                    "ADC by WkdayWkend" = unit_type_wkendwkday_adc,
                    "Census Scenario - 5% Elective" = census_scenarios,
                    "Dept Mappings" = dept_mapping_table,
                    "Admit Type Mapping" = admit_type_mapping)

write_xlsx(export_list,
           path = paste0(user_directory,
                         "/Data/Census Scenario Export ",
                         format(Sys.Date(), "%Y-%m-%d"),
                         ".xlsx"))
