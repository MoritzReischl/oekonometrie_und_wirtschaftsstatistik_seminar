library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(writexl)

country_codes <- tibble::tribble(
  ~country, ~country_code,
  "European Union - 27 countries (from 2020)", "EU27_2020",
  "Belgium", "BE",
  "Bulgaria", "BG",
  "Czechia", "CZ",
  "Denmark", "DK",
  "Germany", "DE",
  "Estonia", "EE",
  "Ireland", "IE",
  "Greece", "EL",
  "Spain", "ES",
  "France", "FR",
  "Croatia", "HR",
  "Italy", "IT",
  "Cyprus", "CY",
  "Latvia", "LV",
  "Lithuania", "LT",
  "Luxembourg", "LU",
  "Hungary", "HU",
  "Malta", "MT",
  "Netherlands", "NL",
  "Austria", "AT",
  "Poland", "PL",
  "Portugal", "PT",
  "Romania", "RO",
  "Slovenia", "SI",
  "Slovakia", "SK",
  "Finland", "FI",
  "Sweden", "SE",
  "Iceland", "IS",
  "Norway", "NO",
  "Switzerland", "CH",
  "Bosnia and Herzegovina", "BA",
  "Montenegro", "ME",
  "North Macedonia", "MK",
  "Albania", "AL",
  "Serbia", "RS"
)

process_eurostat_data <- function(file_path, nace_category_cell, header_row, economic_indicator, target_name) {
  # returns the titles of all sheets in the workbook, exclusive the summary sheet
  data_sheets <- excel_sheets(file_path) %>%
    setdiff("Summary")
  
  # define a function that receives a sheet_name as an argument
  read_economic_indicator_sheet <- function(sheet_name) {
    # extract the category name in cell c7
    nace_category_name <- read_excel(
      file_path,
      sheet = sheet_name,
      range = nace_category_cell,
      col_names = FALSE,
      col_types = "text"
    )[[1]]
  
    # extract the data 
    read_excel(
      file_path,
      sheet = sheet_name,
      # starting at row 10
      skip = header_row - 1,
      # eurostat uses : for empty values
      na = c(":", ""),
      # extract year headers from row 10
      col_names = TRUE,
      # convert all data to text which is later converted to prevent type conflicts downstream
      col_types = "text",
      # columns names must be unique, duplicate names append a suffix number
      .name_repair = "unique"
    ) %>%
      # column name above countries is TIME refering to the other column headers, not first row values
      rename(country = TIME) %>%
      # remove rows with empty country values and the empty GEO row below the headers
      filter(!is.na(country), country != "GEO (Labels)") %>%
      # filter countries not in the above list
      inner_join(country_codes, by = "country") %>%
      # pivot the table from country, 2021, 2022 etc to country, year, gross_operating_rate
      pivot_longer(
        # select only columns whose name starts with 4 digits = years
        cols = matches("^\\d{4}"),
        names_to = "year",
        values_to = economic_indicator
      ) %>%
      # add extracted category name to table and convert year and gross operating rate to integers and floating point numbers respectively
      mutate(
        year = as.integer(str_extract(year, "\\d{4}")),
        nace_category_name = nace_category_name,
        across(all_of(economic_indicator), ~ as.numeric(str_replace_all(.x, ",", "")))
      ) %>%
      # return final table
      select(
        country_code,
        year,
        nace_category_name,
        all_of(economic_indicator)
      )
  }
  
  # apply previously defined function to each sheet in the Eurostat workbook
  economic_indicator_nace <- map_dfr(
    data_sheets,
    read_economic_indicator_sheet
  )
  
  # store the result persistently as an Excel workbook
  writexl::write_xlsx(
    economic_indicator_nace,
    paste0("data/", target_name, ".xlsx")
  )
  
  economic_indicator_nace
}

turnover_2021_2024 = process_eurostat_data(
  file_path = "data/raw/turnover/turnover by NACE Rev. 2 level 3 activities 2021-2024.xlsx",
  nace_category_cell = "C7",
  header_row = 10,
  economic_indicator = "turnover_million_eur",
  target_name = "raw/turnover_by_nace_2.0_level_3_coun_year_2021_24"
)

turnover_2013_2020 = process_eurostat_data(
  file_path = "data/raw/turnover/turnover by NACE Rev. 2 level 3 activities 2013-2020.xlsx",
  nace_category_cell = "C6",
  header_row = 10,
  economic_indicator = "turnover_million_eur",
  target_name = "raw/turnover_by_nace_2.0_level_3_coun_year_2013_20"
)

# union both tables into a single table
turnover_2013_2024 <- bind_rows(turnover_2021_2024, turnover_2013_2020)

writexl::write_xlsx(
  turnover_2013_2024,
  "data/turnover_by_nace_2.0_level_3_coun_year_2013_24.xlsx"
)
