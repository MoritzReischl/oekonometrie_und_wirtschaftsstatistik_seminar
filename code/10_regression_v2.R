library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

normalize <- function(x) {
  x %>%
    str_remove("\\s*\\(en\\)\\s*$") %>%
    str_squish() %>%
    str_to_lower()
}

######
# 2nd regression: use aggregated ETS activities and NACE categories with a 1:1 mapping
######
turnover_file <- "data/2_staging/turnover_2013_24_l2_l3_agg.xlsx"

# use all level mapping
# all nace activities need to be present to:
# A) include both NACE activities that are covered and not covered by ets, 
# B) guarantee that the two groups are mutually exclusive
mapping_agg <- read_excel(
  "data/1_raw/mapping ets nace/mapping_ets_nace_all_levels.xlsx",
  sheet = "Mapping All Nace Activities"
) %>%
  distinct(nace_level, nace_code_agg, nace_label_agg, ets_activity_code_agg, ets_activity_name_agg) %>%
  mutate(
    ets_activity_code = as.character(ets_activity_code_agg),
    nace_label_agg = normalize(nace_label_agg),
    ets_activity_code = as.character(ets_activity_code_agg),
    ets_activity_name_agg = normalize(ets_activity_name_agg)
  )

turnover_2013_2024_agg <- read_excel(turnover_file) %>%
  select(
    country_code,
    year,
    nace_code_agg,
    nace_label_agg,
    turnover_mio_eur
  ) %>% 
  filter(
    !is.na(turnover_mio_eur),
    country_code != "EU27_2020",
    turnover_mio_eur > 0
  )

# add ets_activity_codes to ets net costs
ets_net_agg <- read_excel(
  "data/2_staging/ets_net_cost_agg.xlsx"
) %>%
  mutate(
    ets_activity_code_agg = as.character(ets_activity_code_agg)
  ) %>%
  filter(
    # Other activity opted-in under Art. 24 is hard to map
    ets_activity_code_agg != "99"
  ) %>%
  inner_join(
    mapping_agg %>%
      select(ets_activity_code_agg, nace_code_agg) %>%
      distinct(),
    by = "ets_activity_code_agg"
  ) %>%
  select(
    country_code, 
    year, 
    ets_activity_code_agg,
    ets_activity_name_agg,
    ets_net_cost_eur,
    nace_code_agg
  )

# create final regression data
# 1. join turnover with ets_net_cost
regression_data <- turnover_2013_2024_agg %>%
  # left_join so that later outer_join shows orphans for both ets and turnover data
  left_join(
    ets_net_agg,
    by = c("country_code", "year", "nace_code_agg")
  ) %>%
  mutate(
    # NA ets costs are actually 0 net cost
    ets_net_cost_mio_eur = replace_na(ets_net_cost_eur, 0) / 1000000
  ) %>%
  select(
    country_code,
    year,
    nace_code_agg,
    nace_label_agg,
    turnover_mio_eur,
    ets_activity_code_agg,
    ets_activity_name_agg,
    ets_net_cost_mio_eur
  )

# 2. join with interest rate
interest_rate <- read_excel("data/2_staging/interest_rates.xlsx")

# regression_data <- regression_data %>%
regression_data <- regression_data |>
  left_join(interest_rate, by = c("year","country_code"))

print(n_distinct(turnover_2013_2024_agg$country_code))
print(n_distinct(ets_net_agg$country_code))
print(n_distinct(interest_rate$country_code))
print(n_distinct(regression_data$country_code))

print(n_distinct(turnover_2013_2024_agg$year))
print(n_distinct(ets_net_agg$year))
print(n_distinct(interest_rate$year))
print(n_distinct(regression_data$year))

# 3. add fixed effects
# factor terms are flags of 0 or 1 where only a single value per variable and observation is 1 
# factor terms are controlling for the average turnover of an independent variable's value across all other independent variables, e. g. factor(2020) calculates an average across all sectors and countries on this year. Effect of net_ets_cost is then calculated as the deviations from the average fixed effects
# removes all rows where there are na in the data => previous left_joins are effectively inner_joins
regression_model <- lm(
  log(turnover_mio_eur) ~ ets_net_cost_mio_eur +
    interest_rate_corporations +
    factor(country_code) +
    factor(year) +
    factor(nace_label_agg),
  data = regression_data
)

# summary(regression_model)
coefficients <- coef(
  summary(regression_model)
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term")

model_stats <- tibble::tibble(
  n_observations = nobs(regression_model),
  r_squared = summary(regression_model)$r.squared,
  adjusted_r_squared = summary(regression_model)$adj.r.squared,
  residual_standard_error = summary(regression_model)$sigma
)

writexl::write_xlsx(
  list(
    regression_data = regression_data,
    coefficients = coefficients,
    model_stats = model_stats
  ),
  "data/3_results/regression_results_v2.xlsx"
)

