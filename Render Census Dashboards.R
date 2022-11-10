# This code renders the COVID-19 Vaccine Administration data for the health system
# This dashboard looks at historical doses administered by setting type (hospital POD vs practices)
# and is less focused on scheduled appointments in the coming weeks.

# Clear environment
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


# Render markdown file with dashboard code and save with today's date
rmarkdown::render(here::here("System Wide Census Scenarios.Rmd"), 
                  output_file = paste0(
                    user_directory,
                    "/Dashboards",
                    "/MSHS Census Scenarios ",     
                    format(Sys.Date(), "%m-%d-%y")))