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

turnover_file <- "data/turnover_2013_24_l3.xlsx"

# use level 3 mapping only
ets_activity_nace_level3_mapping <- read_excel(
  "data/raw/mapping ets nace/mapping_ets_nace_level3.xlsx",
  sheet = "Mapping"
) %>%
  mutate(
    ets_activity_code = as.character(ets_activity_code),
    nace_join_key = normalize(nace_level_3_label)
  ) %>%
  select(
    ets_activity_code,
    nace_level_3_code,
    nace_join_key,
    nace_level_3_label,
  ) %>%
  distinct()

turnover_2013_2024 <- read_excel(turnover_file) %>%
  mutate(
    nace_join_key = normalize(nace_category_name),
    turnover_eur = turnover_million_eur * 1000000
  ) %>%
  select(
    country_code,
    year,
    nace_category_name,
    nace_join_key,
    turnover_eur
  ) %>% 
  filter(
    turnover_eur > 0,
    country_code != "EU27_2020"
  )

# check if all mapped nace labels appear in the turnover table
mapped_nace_with_turnover_match <- ets_activity_nace_level3_mapping %>%
  distinct(
    nace_level_3_label,
    nace_join_key
  ) %>%
  mutate(
    mapping_nace_label = nace_level_3_label,
    mapping_nace_key = nace_join_key
  ) %>%
  left_join(
    turnover_2013_2024 %>%
      distinct(
        nace_category_name,
        nace_join_key
      ),
    by = "nace_join_key"
  ) %>%
  mutate(
    turnover_nace_label = nace_category_name,
    turnover_nace_key = nace_join_key
  ) %>%
  select(
    mapping_nace_key,
    mapping_nace_label,
    turnover_nace_label,
    turnover_nace_key
  ) %>%
  arrange(mapping_nace_key, turnover_nace_key)

print(n = 100, mapped_nace_with_turnover_match)

# add ets_activity_codes to ets net costs
ets_net_cost_by_year_coun_acti_labeled <- read_excel(
  "data/ets_net_cost.xlsx"
) %>%
  mutate(
    ets_activity_code = as.character(ets_activity_code)
  ) %>%
  left_join(
    ets_activity_nace_level3_mapping,
    by = "ets_activity_code",
    relationship = "many-to-many"
  ) %>%
  filter(
    !is.na(ets_net_cost_eur)
  )

# create final regression data
# 1. join turnover with ets_net_cost
regression_data <- turnover_2013_2024 %>%
  left_join(
    ets_net_cost_by_year_coun_acti_labeled,
    by = c("country_code", "year", "nace_join_key")
  ) %>%
  mutate(
    # NA ets costs are actually 0 net cost
    ets_net_cost_eur_mio = replace_na(ets_net_cost_eur, 0) / 1000000,
    turnover_eur_mio = turnover_eur / 1000000
  ) %>%
  select(
    country_code,
    year,
    nace_category_name,
    ets_activity_code,
    ets_activity_name,
    nace_join_key,
    turnover_eur_mio,
    ets_price_mean_eur_tCO2,
    emissions_tonne_CO2_equi,
    freely_allocated_corrected_allowances_tonne_CO2_equi,
    net_allowance_obligation_tonne_CO2_equi,
    ets_net_cost_eur_mio
  )

# 2. join with interest rate
# interest_rate_3m_yearly <- read_excel("data/interest_rate_3m.xlsx")

# regression_data <- regression_data %>%
#   left_join(interest_rate_3m_yearly, by = "year")

# 3. add fixed effects
# factor terms are flags of 0 or 1 where only a single value per variable and observation is 1 
# factor terms are controlling for the average turnover of an independent variable's value across all other independent variables, e. g. factor(2020) calculates an average across all sectors and countries on this year. Effect of net_ets_cost is then calculated as the deviations from the average fixed effects
regression_model <- lm(
  log(turnover_eur_mio) ~ ets_net_cost_eur_mio +
    # interest_rate_3m +
    factor(country_code) +
    factor(year) +
    factor(nace_category_name),
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
  "data/regression_results.xlsx"
)

######
# 2nd regression: use aggregated ETS activities and NACE categories with a 1:1 mapping
######
turnover_file <- "data/turnover_2013_24_l2_l3_agg.xlsx"

# use all level mapping
# all nace activities need to be present to:
# A) include both NACE activities that are covered and not covered by ets, 
# B) guarantee that the two groups are mutually exclusive
mapping_agg <- read_excel(
  "data/raw/mapping ets nace/mapping_ets_nace_all_levels.xlsx",
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
  "data/ets_net_cost_agg.xlsx"
) %>%
  mutate(
    ets_activity_code_agg = as.character(ets_activity_code_agg)
  ) %>%
  filter(
    # Other activity opted-in under Art. 24 is hard to map
    ets_activity_code_agg != "99"
  ) %>%
  # left_join so that later outer_join shows orphans for both ets and turnover data
  left_join(
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
# interest_rate_3m_yearly <- read_excel("data/interest_rate_3m_yearly.xlsx")

# regression_data <- regression_data %>%
#   left_join(interest_rate_3m_yearly, by = "year")

# 3. add fixed effects
# factor terms are flags of 0 or 1 where only a single value per variable and observation is 1 
# factor terms are controlling for the average turnover of an independent variable's value across all other independent variables, e. g. factor(2020) calculates an average across all sectors and countries on this year. Effect of net_ets_cost is then calculated as the deviations from the average fixed effects
regression_model <- lm(
  log(turnover_mio_eur) ~ ets_net_cost_mio_eur +
    # interest_rate_3m +
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
  "data/regression_results.xlsx"
)

