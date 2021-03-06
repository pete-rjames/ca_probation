# SUMMARISE 2014 PROBATION CASELOAD DATA BY COUNTY AND CREATE MAP

# Library

library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)

# Step 1: read in dataframe 

df <- read.csv("https://raw.githubusercontent.com/pete-rjames/ca_probation/master/ca_oag_probation_2005-2014.csv", header = TRUE)

# Step 2: subset data frame

df_2014 <- df %>%
  filter(report == "Caseload" & year == 2014) %>%
  spread(offense, value) %>%
  clean_names() %>%
  mutate(state_percent = 100*(total/sum(total))) %>%
  select(county, felony, misdemeanor, total, state_percent)

write.csv(df_2014, file = "ca_oag_probation_caseload_2014.csv", row.names = FALSE)

# Step 3: create data frame for map

# Formatting caseload data frame

df_2014_map_format <- df_2014 %>%
  mutate(county = tolower(county)) %>%
  mutate(state_percent = round(state_percent,1)) %>%
  rename(county.name = county, value = state_percent) %>%
  select(county.name, value)

# Importing CA counties data frame

library(choroplethrMaps)
data(county.regions)

ca_county <- county.regions %>%
  filter(state.abb == "CA") %>%
  select(region, county.name)

# Joining caseload dataframe to CA counties

df_2014_map <- left_join(ca_county, df_2014_map_format, by = "county.name") %>%
  select(region, value)

# Step 4: produce choropleth map

library(choroplethr)
county_choropleth(df_2014_map,
                  legend = "state_percent",
                  title = "County percentage share of state probation population, 2014",
                  state_zoom = "california")

ggsave(filename="ca_probation_map.png")
