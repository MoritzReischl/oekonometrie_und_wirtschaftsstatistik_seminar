library(readxl)
library(dplyr)
library(writexl)
library(here)

interest_rate_file <- here(
  "data/1_raw/interest rates/interest rates ECB EU countries.xlsx"
)

# Read monthly ECB cost-of-borrowing rates for non-financial corporations.
# The DATA(MIR) sheet contains one series per country and monthly observations
# measured in percent per annum.
interest_rates_ecb_monthly <- read_excel(
  interest_rate_file,
  sheet = "DATA(MIR)"
) |>
  transmute(
    date = as.Date(.data[["DATE"]]),
    country_code = .data[["REFERENCE AREA"]],
    interest_rate_corporations = as.numeric(.data[["OBS.VALUE"]])
  ) |>
  filter(
    !is.na(date),
    !is.na(country_code),
    !is.na(interest_rate_corporations)
  )

# Convert the monthly observations to annual country-level means.
interest_rates_ecb_yearly <- interest_rates_ecb_monthly |>
  mutate(year = as.integer(format(date, "%Y"))) |>
  mutate(
    country_code = if_else(country_code == "GR", "EL", country_code)
  ) |>
  filter(country_code != "U2") |>
  group_by(country_code, year) |>
  summarise(
    interest_rate_corporations = mean(
      interest_rate_corporations,
      na.rm = TRUE
    ),
    months_observed = n(),
    .groups = "drop"
  ) |>
  arrange(country_code, year)

write_xlsx(
  interest_rates_ecb_yearly,
  here("data/2_staging/interest_rates.xlsx")
)
