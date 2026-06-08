library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)

file <- "data/raw/turnover_in_industry_eu_2013_2026_03.xlsx"

sheets <- excel_sheets(file)
data_sheets <- sheets[sheets != "Summary"]

read_turnover_sheet <- function(sheet_name) {
  
  # Read metadata from top rows
  meta <- read_excel(
    file,
    sheet = sheet_name,
    col_names = FALSE,
    n_max = 9
  )
  
  industry <- meta %>%
    filter(`...1` == "Statistical classification of economic activities in the European Community (NACE Rev. 2)") %>%
    pull(`...3`)
  
  seasonal_adjustment <- meta %>%
    filter(`...1` == "Seasonal adjustment") %>%
    pull(`...3`)
  
  unit <- meta %>%
    filter(`...1` == "Unit of measure") %>%
    pull(`...3`)
  
  # Read actual data table
  df <- read_excel(
    file,
    sheet = sheet_name,
    skip = 10,
    .name_repair = "unique"
  )
  
  df %>%
    rename(country = TIME) %>%
    filter(!is.na(country)) %>%
    filter(country != "GEO (Labels)") %>%
    
    # Keep only real month columns, ignore empty flag columns
    pivot_longer(
      cols = matches("^\\d{4}-\\d{2}$"),
      names_to = "month_date",
      values_to = "turnover"
    ) %>%
    
    mutate(
      turnover = na_if(as.character(turnover), ":"),
      turnover = parse_number(turnover),
      month_date = ym(month_date),
      year = year(month_date),
      month = month(month_date),
      industry = industry,
      seasonal_adjustment = seasonal_adjustment,
      unit = unit,
      sheet = sheet_name
    ) %>%
    
    select(
      country,
      industry,
      seasonal_adjustment,
      year,
      month,
      month_date,
      turnover,
      unit,
      sheet
    )
}

turnover_long <- map_dfr(data_sheets, read_turnover_sheet)
