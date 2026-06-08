library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(tidyr)
library(stringr)

file_path <- "data/raw/turnover_operating_surplus_NACE_annually.xlsx"
# create list of excel sheets in the workbook, but skip the first sheet
combined_data <- excel_sheets(file_path)[-1] %>%
  # for each sheet in the list, apply function
  map_dfr(
    ~ {
      # collect the NACE category of the current sheet
      sheet_metadata <- read_excel(
        file_path,
        # which sheet? The xth iteration.
        sheet = .x,
        # each sheet = 1 NACE category, name at C7
        range = "C6:C7",
        # no headers, so that the cell value is treated as data, not column name
        col_names = FALSE,
        # no error-prone data type auto-detection, e.g. double for "Belgium" => requires explicit typecasting in the end
        col_types = "text"
      )
      # C6 is either "Gross operating surplus - million euro" or "Net turnover - million euro". . Last [1] to extract the value and not matrix
      variable_raw <- sheet_metadata[1,1][[1]]
      # Rename variable to concise name or stop with error if unexpected indicator
      if (variable_raw == "Gross operating surplus - million euro") {
        variable <- "gross_operating_surplus"
      } else if (variable_raw == "Net turnover - million euro") {
        variable <- "net_turnover"
      } else {
        stop(paste("Unexpected variable in sheet", .x, ":", variable_raw))
      }
      
      # C7 is the NACE category. Last [1] to extract the value and not matrix
      nace_activity <- sheet_metadata[2,1][[1]]     
      
      # iterate over all sheets in the excel
      read_excel(
        file_path,
        # xth sheet iteration
        sheet = .x,
        # country names as headers start at row 9
        skip = 9,
        na = c(":", ""),
        col_names = TRUE,
        col_types = "text"
      ) %>% 
        
      # unify headers to snake_case
      clean_names() %>%
      
      # first row after the country header is the "TIME" row
      filter(geo_labels != "TIME") %>%
      
      # year is stored in the first column
      rename(year = geo_labels) %>%
      
      # remove flag columns such as 3, 5, etc.
      select(
        year,
        where(~ !all(is.na(.x)))
      ) %>%

      # reorganize to default structure: (nace_activity) | year | country | variable 
      pivot_longer(
        # retains year as the identifier column as is and pivots all other columns
        cols = -year,
        # headers are now a new column country
        names_to = "country",
        # moves old cell values into a new column called by identifier
        values_to = variable
      ) %>%
        
      # add nace_activity as first column in new table and standardize format: nace_activity | year | country | variable
      mutate(
        nace_activity = nace_activity,
        year = as.integer(year),
        country = str_replace_all(country, "_", " "),
        # modifies the variable column of the sheet (C6)
        # removes , so that numbers only contain . as delimiter for floating point
        # converts potential text to numeric
        "{variable}" := as.numeric(str_replace_all(.data[[variable]], ",", ""))
      ) %>%
        
      select(nace_activity, year, country, all_of(variable))
    }
  )

writexl::write_xlsx(
  combined_data,
  "data/net_turnover_gross_operating_surplus.xlsx"
)
