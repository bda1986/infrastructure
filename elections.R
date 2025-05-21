#install.packages("gerda")

# Load the package
library(gerda)
library(dplyr)

# List available datasets
available_data <- gerda_data_list()
available_data

# Load county dataset
datam <- load_gerda_web("municipal_harm", verbose = TRUE, file_format = "rds")

# Describe data
head(data)
names(data)

# Create ARS variable by renaming county_code (for matching)
data <- data %>%
  rename(AGS = county_code)

# Create a new dataset only including last election
data_latest <- data %>%
  group_by(AGS) %>%
  filter(election_year == max(election_year, na.rm = TRUE)) %>%
  ungroup()



