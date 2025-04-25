#install.packages("gerda")

# Load the package
library(gerda)
library(dplyr)

# List available datasets
available_data <- gerda_data_list()
available_data

# Load county dataset
data_cty_harm <- load_gerda_web("federal_cty_harm", verbose = TRUE, file_format = "rds")

# Describe data
head(data_cty_harm)
names(data_cty_harm)

# Create ARS variable by renaming county_code (for matching)
data_cty_harm <- data_cty_harm %>%
  rename(AGS = county_code)

# Create a new dataset only including last election
data_cty_latest <- data_cty_harm %>%
  group_by(AGS) %>%
  filter(election_year == max(election_year, na.rm = TRUE)) %>%
  ungroup()



