library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

turnover_file <- "data/turnover_by_nace_2.0_level_3_coun_year_2013_24.xlsx"

if (!file.exists(turnover_file)) {
  stop(
    paste(
      turnover_file,
      "does not exist yet. Run 'staging economic indicator NACE.R' first."
    )
  )
}

normalize_nace_label <- function(x) {
  x %>%
    str_remove("\\s*\\(en\\)\\s*$") %>%
    str_squish() %>%
    str_to_lower()
}

ets_activity_nace_level3_mapping <- read_excel(
  "data/raw/mapping_ets_activity_nace_level3.xlsx",
  sheet = "Mapping"
) %>%
  mutate(
    ets_activity_code = as.character(ets_activity_code),
    nace_join_key = normalize_nace_label(nace_level_3_label)
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
    nace_join_key = normalize_nace_label(nace_category_name),
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
    !is.na(turnover_eur),
    turnover_eur != 0,
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

ets_net_cost_by_year_coun_acti_labeled <- read_excel(
  "data/ets_net_cost_by_year_coun_acti_labeled.xlsx"
) %>%
  mutate(
    ets_activity_code = as.character(ets_activity_code)
  ) %>%
  left_join(
    ets_activity_nace_level3_mapping,
    by = "ets_activity_code"
  ) %>%
  filter(
    !is.na(ets_net_cost_eur)
  )

turnover_ets_net_cost_regression_data <- turnover_2013_2024 %>%
  left_join(
    ets_net_cost_by_year_coun_acti_labeled,
    by = c("country_code", "year", "nace_join_key")
  ) %>%
  mutate(
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

# factor terms are flags of 0 or 1 where only a single value per variable and observation is 1 
# factor terms are controlling for the average turnover of an independent variable's value across all other independent variables, e. g. factor(2020) calculates an average across all sectors and countries on this year. Effect of net_ets_cost is then calculated as the deviations from the average fixed effects
turnover_ets_net_cost_model <- lm(
  log(turnover_eur_mio) ~ ets_net_cost_eur_mio +
    factor(country_code) +
    factor(year) +
    factor(nace_category_name),
  data = turnover_ets_net_cost_regression_data
)

# summary(turnover_ets_net_cost_model)

turnover_ets_net_cost_coefficients <- coef(
  summary(turnover_ets_net_cost_model)
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term")

turnover_ets_net_cost_model_stats <- tibble::tibble(
  n_observations = nobs(turnover_ets_net_cost_model),
  r_squared = summary(turnover_ets_net_cost_model)$r.squared,
  adjusted_r_squared = summary(turnover_ets_net_cost_model)$adj.r.squared,
  residual_standard_error = summary(turnover_ets_net_cost_model)$sigma
)

writexl::write_xlsx(
  list(
    regression_data = turnover_ets_net_cost_regression_data,
    coefficients = turnover_ets_net_cost_coefficients,
    model_stats = turnover_ets_net_cost_model_stats
  ),
  "data/regression turnover ETS net cost.xlsx"
)
