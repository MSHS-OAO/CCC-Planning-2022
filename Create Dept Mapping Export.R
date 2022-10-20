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

## Import census data to start creating unit mappings
raw_df <- read_excel(path = paste0(user_directory,
                                   "/Data/Census Data 3.1.21-5.31.21.xlsx"))

census_depts <- raw_df %>%
  select("Ip Site", "Census Dept") %>%
  distinct() %>%
  rename(Site = "Ip Site",
         Dept = "Census Dept") %>%
  arrange(Site, Dept)

write_xlsx(census_depts,
           path = paste0(user_directory,
                         "/Data/Census Dept Mappings 2022-10-18.xlsx"))

