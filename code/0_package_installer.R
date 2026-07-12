# Packages required by scripts 1-11. Only packages that are actually missing
# are installed, so rerunning this setup script is safe and fast.
required_packages <- c(
  "janitor", "readxl", "dplyr", "purrr", "tidyr", "stringr",
  "writexl", "openxlsx", "tidyverse", "here", "lubridate",
  "ggplot2", "scales", "igraph", "fixest"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0L) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
