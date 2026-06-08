library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)

####### Step 1: Join all the yearly ETS spot primary auction data into one file
folder <- "data/raw/ETS EEX spot primary 2012-2025-data"

# Stores full paths of the files in the folder of type xls or slsx
files <- list.files(
  path = folder,
  pattern = "\\.xls[x]?$",
  full.names = TRUE
)

# %>% = take the result on the left and return it as input on the right
ets_auction_data <- files %>%
  set_names(basename(.)) %>%
  # joins all elements of a list to a single table
  map_dfr(
    function(file) {
      year <- str_extract(basename(file), "\\d{4}") %>% as.integer()
      rows_to_skip <- if(year >= 2012 && year < 2017){
        2
      } else{
        5
      }
      
      read_excel(file, skip = rows_to_skip)
    },
    # adds the column id for traceability
    .id = "source_file"
  )

###### Step 2: Calculate mean over years
ets_auction_yearly_data <- ets_auction_data %>%
  mutate(auction_year = year(Date)) %>%
  group_by(auction_year) %>%
  summarise(
    `€/tCO2 Mean` = mean(`Auction Price €/tCO2`, na.rm = TRUE),
    `€/tCO2 Median` = median(`Auction Price €/tCO2`, na.rm = TRUE),
    `Volume tCO2 Total` = sum(`Auction Volume tCO2`, na.rm = TRUE),
    `Revenue Total` = sum(`Total Revenue €`, na.rm = TRUE),
    auctions_count = n(),
    # drop grouping from dataframe to prevent unintended grouping in the future
    .groups = "drop"
  )

###### Step 3: Store result as Excel as persistent backup
library(writexl)
writexl::write_xlsx(
  ets_auction_yearly_data,
  "data/ets_auction_yearly_data_2012_2025.xlsx"
)

###### Step 4: Plot results
ggplot(ets_auction_yearly_data, aes(x = auction_year, y = `€/tCO2 Mean`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = ets_auction_yearly_data$auction_year
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 10)
  ) +
  labs(
    title = "Average ETS auction price per year",
    x = "Year",
    y = "Mean Auction Price €/tCO2"
  ) +
  theme(
    plot.title = element_text(
      size = 20,
      hjust = 0.5
    ),
    axis.title.x = element_text(size=14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
