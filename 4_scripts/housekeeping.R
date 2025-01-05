# Check if pacman is installed, install if necessary
suppressPackageStartupMessages(suppressMessages(
  suppressWarnings(if (!require(pacman)) {
  install.packages('pacman', dependencies = TRUE)
})))

# Use pacman to install and load packages
pacman::p_load(
  dplyr,
  conflicted
)

# Utility functions
pacman::p_load_gh('ChrisDonovan307/projecter')

conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::summarize()
)

cat('\n* Housekeeping complete *\n')
