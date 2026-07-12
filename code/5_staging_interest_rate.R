library(readxl)
library(dplyr)
library(writexl)
library(here)

interest_rate_file <- here("data/raw/interest rates/interest rate eurostat yearly irt_st_m_22063167.xlsx")

# Read Eurostat money market interest rates (annual, Euro area)
# Sheet layout: skip 7 rows of metadata; row 1 is the label row (dropped via slice(-1))
# Column layout: ...2 = TIME (year), ...7 = 3-month rate
interest_rate_3m_yearly <- read_excel(
  interest_rate_file, sheet = "Sheet 1", skip = 7, col_names = FALSE
) |>
  slice(-1) |>
  select(time = ...2, rate_3m = ...7) |>
  filter(!is.na(time), !is.na(rate_3m), rate_3m != ":") |>
  mutate(
    year = as.integer(time),
    interest_rate_3m = as.numeric(rate_3m)
  ) |>
  select(year, interest_rate_3m)

write_xlsx(interest_rate_3m_yearly, "data/interest_rate_3m.xlsx")
