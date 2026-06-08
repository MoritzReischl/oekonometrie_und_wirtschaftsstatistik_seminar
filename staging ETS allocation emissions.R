library(dplyr)

# import dataset 

create_plot <- function(title, dataset, x_field, x_name, y_field, y_name, group_field, group_name) {
  x_values <- dataset %>% pull({{ x_field }})
  
  ggplot(
    dataset,
    aes(
      x = {{ x_field }},
      y = {{ y_field }},
      color = as.factor({{ group_field }}),
      group = {{ group_field }}
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = seq(
        min(x_values, na.rm = TRUE),
        max(x_values, na.rm = TRUE),
        by = 2
      )
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale())
    ) +
    labs(
      title = title,
      x = x_name,
      y = y_name,
      color = group_name
    ) +
    theme(
      plot.title = element_text(
        size = 20,
        hjust = 0.5
      ),
      axis.title.x = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )
}

###### Step 1: Eploration of data
unique(ets_allocated_2005_to_2024_fully$country_code)

exclude_non_country_codes <- c(
  "Innovation fund",
  "Modernisation Fund",
  "NER 300 auctions", # 
  "RRF"
)

non_EU_country_codes <- c(
  "GB", # GB not in EU27
  "IS", # not in EU
  "LI", # Liechtenstein
  "NO", # Norway
  "XI" # Northern Ireland
)

# only one value: "all entitites" => can be ignored, no filtering
unique(ets_allocated_2005_to_2024_fully$active_installation)
unique(ets_allocated_2005_to_2024_fully$citl_information)
unique(ets_allocated_2005_to_2024_fully$unit)
unique(ets_allocated_2005_to_2024_fully$year)

activity_code_groups = c("20-99", "21-99")

# import the labels of the ets main_activity_codes
excel_sheets("data/raw/ets_activity_to_NACE_sector_mapping.xlsx")
ets_activity_code_labels <- read_excel("data/raw/ets_activity_to_NACE_sector_mapping.xlsx",
                                       sheet="EU ETS Data Viewer")

###### dataset 1: freely_allocated allowances with corrections by year, country and activity
# filter dataset by only country-codes and group by country_code, year and main_activity_code 
ets_freely_allocated_by_year_coun_acti <- ets_allocated_2005_to_2024_fully %>%
  filter(
    !country_code %in% exclude_non_country_codes,
    citl_information == "1.1 Freely allocated allowances",
    !str_detect(year, "^Total"),
    !main_activity_code %in% activity_code_groups
  ) %>%
  mutate(year = as.integer(year)) %>%
  group_by(country_code, year, main_activity_code) %>%
  summarise(
    freely_allocated_allowances_tonne_CO2_equi = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) 

# aggregate corrections per country, year and activity_code
ets_corrections_by_year_coun_acti <- ets_allocated_2005_to_2024_fully %>%
  filter(
    !country_code %in% exclude_non_country_codes,
    citl_information == "1.2 Correction to freely allocated allowances (not reflected in EUTL)",
    !str_detect(year, "^Total"),
    !main_activity_code %in% activity_code_groups
  ) %>%
  mutate(year = as.integer(year)) %>%
  group_by(country_code, year, main_activity_code) %>%
  summarise(
    corrections_allowances_tonne_CO2_equi = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# sum of freely allocated values and corrections
ets_freely_allocated_corr_by_year_coun_acti <- ets_freely_allocated_by_year_coun_acti %>%
  left_join(
    ets_corrections_by_year_coun_acti,
    by = c("country_code", "year", "main_activity_code")
  ) %>%
  mutate(
    corrections_allowances_tonne_CO2_equi_tidy = tidyr::replace_na(corrections_allowances_tonne_CO2_equi, 0),
    freely_allocated_corrected_allowances_tonne_CO2_equi =
      freely_allocated_allowances_tonne_CO2_equi + corrections_allowances_tonne_CO2_equi_tidy
  )

# store corrected freely allocated allowances persistently
library(writexl)
writexl::write_xlsx(
  ets_freely_allocated_corr_by_year_coun_acti,
  "data/ets_freely_allocated_corr_by_year_coun_acti.xlsx"
)

# add labels to freely allocated allowances
ets_freely_allocated_corr_by_year_coun_acti_labeled <- 
  ets_freely_allocated_corr_by_year_coun_acti %>%
  mutate(main_activity_code = as.character(main_activity_code)) %>%
  left_join(
    ets_activity_code_labels,
    by = c("main_activity_code" = "activity_code")
  )

# reduce number of output graphs
top_allocated_activities <- ets_freely_allocated_corr_by_year_coun_acti_labeled %>%
  group_by(main_activity_code, activity_name) %>%
  summarise(
    total_allocated_all_years = sum(
      freely_allocated_corrected_allowances_tonne_CO2_equi,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  slice_max(total_allocated_all_years, n = 10)

ets_freely_allocated_corr_by_year_coun_acti_labeled_10 <- 
  ets_freely_allocated_corr_by_year_coun_acti_labeled %>%
  semi_join(
    top_allocated_activities,
    by = c("main_activity_code", "activity_name")
  )

# reduce to year x activity
ets_freely_allocated_corr_by_year_acti_labeled_10 <- 
  ets_freely_allocated_corr_by_year_coun_acti_labeled_10 %>%
  group_by(year, main_activity_code, activity_name) %>%
  summarise(
    total_freely_allocated = sum(
      freely_allocated_corrected_allowances_tonne_CO2_equi,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# order legend by freely allocated allowances
legend_order <- ets_freely_allocated_corr_by_year_acti_labeled_10 %>%
  group_by(activity_name) %>%
  summarise(
    total_freely_allocated_all_years = sum(total_freely_allocated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_freely_allocated_all_years)) %>%
  pull(activity_name)

ets_freely_allocated_corr_by_year_acti_labeled_10 <- 
  ets_freely_allocated_corr_by_year_acti_labeled_10 %>%
  mutate(
    activity_name = factor(activity_name, levels = legend_order)
  )

# plot
create_plot("Freely allocated allowances by activity over years", ets_freely_allocated_corr_by_year_acti_labeled_10, year, "Year", total_freely_allocated, "Freely allocated allowances tCO2 equi.", activity_name, "Activity name")

###### dataset 2: emissions by year, country and activity
ets_emissions_by_year_coun_acti <- ets_allocated_2005_to_2024_fully %>%
  filter(
    !country_code %in% exclude_non_country_codes,
    citl_information == "2. Verified emissions",
    !str_detect(year, "^Total"),
    !main_activity_code %in% activity_code_groups
  ) %>%
  mutate(year = as.integer(year)) %>%
  group_by(country_code, year, main_activity_code) %>%
  summarise(
    emissions_tonne_CO2_equi = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# check why there are dubious NA values in the plot => likely due to join-errors in 20-45 & 99, 21-45 & 99
# ets_emissions_by_year_coun_acti %>%
#   mutate(main_activity_code = as.character(main_activity_code)) %>%
#   anti_join(
#     ets_activity_code_labels %>%
#       mutate(activity_code = as.character(activity_code)),
#     by = c("main_activity_code" = "activity_code")
#   ) %>%
#   distinct(main_activity_code) %>%
#   arrange(main_activity_code)
# 
# codes_in_labels_not_in_emissions <- ets_activity_code_labels %>%
#   mutate(activity_code = as.character(activity_code)) %>%
#   distinct(activity_code, activity_name) %>%
#   anti_join(
#     ets_emissions_by_year_coun_acti %>%
#       mutate(main_activity_code = as.character(main_activity_code)) %>%
#       distinct(main_activity_code),
#     by = c("activity_code" = "main_activity_code")
#   ) %>%
#   arrange(activity_code)
# codes_in_labels_not_in_emissions

# add labels to code
ets_emissions_by_year_coun_acti_labeled <- left_join(
  ets_emissions_by_year_coun_acti,
  ets_activity_code_labels,
  by = c("main_activity_code" = "activity_code")
)

# store emissions persistently
writexl::write_xlsx(
  ets_emissions_by_year_coun_acti_labeled,
  "data/ets_emissions_by_year_coun_acti.xlsx"
)

###### Plotting

# reduce number of output graphs
top_activities <- ets_emissions_by_year_coun_acti_labeled %>%
  group_by(main_activity_code, activity_name) %>%
  summarise(
    total_emissions_all_years = sum(emissions_tonne_CO2_equi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(total_emissions_all_years, n = 10)

ets_emissions_by_year_coun_acti_labeled_10 = ets_emissions_by_year_coun_acti_labeled %>% 
  semi_join(
    top_activities,
    by = c("main_activity_code", "activity_name")
  )

# for visualization: reduce four dimensions to three: main_activity in relation to emissions per year
ets_emissions_by_year_acti_labeled_10 <- ets_emissions_by_year_coun_acti_labeled_10 %>%
  group_by(year, main_activity_code, activity_name) %>%
  summarise(
    # remove missing values before calculation
    total_emissions_tonne_CO2_equi = sum(emissions_tonne_CO2_equi, na.rm = TRUE),
    .groups = "drop"
  )

# order legend by emissions
legend_order <- ets_emissions_by_year_acti_labeled_10 %>%
  group_by(activity_name) %>%
  summarise(
    total_emissions_all_years = sum(total_emissions_tonne_CO2_equi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_emissions_all_years)) %>%
  pull(activity_name)

ets_emissions_by_year_acti_labeled_10 <- ets_emissions_by_year_acti_labeled_10 %>%
  mutate(
    activity_name = factor(activity_name, levels = legend_order)
  )

# plot
create_plot("Emissions by main activity over years ex. fuel combustion", ets_emissions_by_year_acti_labeled_10, year, "Year", total_emissions_tonne_CO2_equi, "Emissions tCO2", activity_name, "Activity name")

###### 3. dataset: net CO2 allowances = emissions - freely allocated allowances per year, activity and country
ets_net_obligation_by_year_coun_acti <- ets_emissions_by_year_coun_acti %>%
  left_join(
    ets_freely_allocated_corr_by_year_coun_acti,
    by = c("country_code", "year", "main_activity_code")
  ) %>%
  mutate(
    freely_allocated_corrected_allowances_tonne_CO2_equi =
      tidyr::replace_na(freely_allocated_corrected_allowances_tonne_CO2_equi, 0),
    net_allowance_obligation_tonne_CO2_equi =
      emissions_tonne_CO2_equi - freely_allocated_corrected_allowances_tonne_CO2_equi
  )

####### 4. dataset: net CO2 allowance costs per activity, year and country
ets_net_cost_by_year_coun_acti <- ets_net_obligation_by_year_coun_acti %>%
  inner_join(
    ets_auction_yearly_data %>%
      select(
        year = auction_year,
        ets_price_mean_eur_tCO2 = `€/tCO2 Mean`
      ),
    by = "year"
  ) %>%
  mutate(
    net_ets_cost_eur =
      ets_price_mean_eur_tCO2 * net_allowance_obligation_tonne_CO2_equi
  )

# add activity label
ets_net_cost_by_year_coun_acti_labeled <- ets_net_cost_by_year_coun_acti %>%
  mutate(main_activity_code = as.character(main_activity_code)) %>%
  left_join(
    ets_activity_code_labels %>%
      mutate(activity_code = as.character(activity_code)) %>%
      distinct(activity_code, .keep_all = TRUE),
    by = c("main_activity_code" = "activity_code")
  )

# reduce four dimensions to three by aggregating over all countries for plotting
ets_net_cost_by_year_acti <- ets_net_cost_by_year_coun_acti_labeled %>%
  group_by(year, main_activity_code, activity_name) %>%
  summarise(
    total_emissions_tonne_CO2_equi =
      sum(emissions_tonne_CO2_equi, na.rm = TRUE),
    
    total_free_allocation_tonne_CO2_equi =
      sum(freely_allocated_corrected_allowances_tonne_CO2_equi, na.rm = TRUE),
    
    total_net_allowance_obligation_tonne_CO2_equi =
      sum(net_allowance_obligation_tonne_CO2_equi, na.rm = TRUE),
    
    total_net_ets_cost_eur =
      sum(net_ets_cost_eur, na.rm = TRUE),
    
    .groups = "drop"
  )

top_net_cost_activities <- ets_net_cost_by_year_acti %>%
  group_by(main_activity_code, activity_name) %>%
  summarise(
    total_net_ets_cost_all_years = sum(total_net_ets_cost_eur, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(total_net_ets_cost_all_years, n = 10)

ets_net_cost_by_year_acti_10 <- ets_net_cost_by_year_acti %>%
  semi_join(
    top_net_cost_activities,
    by = c("main_activity_code", "activity_name")
  )

create_plot("Net ETS cost by activity over years", ets_net_cost_by_year_acti_10, year, "Year", total_net_ets_cost_eur, "Net ETS cost €", activity_name, "Activity name") + scale_y_log10(labels = label_number(scale_cut = cut_short_scale()))
