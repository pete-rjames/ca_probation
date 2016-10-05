# MAKE CA OAG PROBATION CASELOAD/ACTIONS TIME SERIES 2005 - 2014
# Source: https://oag.ca.gov/crime/cjsc/stats/adult-probation
# Source data includes 58 csv files in: ca_oag_probation_2005-2014.zip
# Note: source data is per county but named via download series position (i.e. county index - 1)

# Library

library(stringr)
library(tidyr)
library(dplyr)

# Step 1: create county index

county_index <- seq(from = 0, to = 57, by = 1)

# Step 3: create variable names

probation_template <- read.csv("./ca_oag_probation_2005-2014/oag_probation_template.csv", 
                               stringsAsFactors = FALSE, 
                               na.strings = c(""),
                               header = TRUE)

# Step 2: create function to read in each csv

load_data <- function(x) {
  csv.name <- paste0("./ca_oag_probation_2005-2014/csv_export (",x,").csv") # csv.name based on download sequence position
  df <- read.csv(csv.name, header = FALSE, skip = 1, stringsAsFactors = FALSE) 
  county <- df[1,1] # extract county name
  county <- gsub("County", "", county) # remove redundant descriptor
  county <- str_trim(county) # trim string
  df <- df[2:11] # removing redundant/empty columns
  df <- df[-(c(1:3, 7,13,19)),] # removing redundant rows
  df$index <- x + 1 # adding index position + 1 to df (alphabetical series CA county)
  df$county <- county # adding county name
  df <- cbind.data.frame(probation_template[1:3], df) # adding variable names to df
  df <- gather(df, variable, value, V2:V11) # convert df wide to long
  df$year <- as.numeric(sub("V", "", df$variable)) + 2003 # create year variable
  df <- select(df, index, county, report, offense, action, year, value) # select/order cols
  stopifnot(dim(df) == c(180,7)) # check dimensions (18 reports * 10 years)
  return(df) # return object
}   

# Step 3: apply function 

list_data <- lapply(0:57, load_data)

# Step 4: aggregate data from list and format

df <- do.call(rbind.data.frame, list_data)
df[1:5] <- lapply(df[1:5], as.factor) 

# Step 5: test assumptions for final dataset 

# check dimensions (18 reports * 10 years * 58 counties)
stopifnot(nrow(df) == 18*10*58) 

# run NA checks
na.check <- function(x) {
  ifelse(sum(is.na(x)==0), "Zero NA", "Stop")
}
lapply(df[c(1:4, 6:7)], na.check) 

# save file
save(df, file = "ca_oag_probation_2005-2014.Rdata")
write.csv(df, file = "ca_oag_probation_2005-2014.csv", row.names = FALSE)
