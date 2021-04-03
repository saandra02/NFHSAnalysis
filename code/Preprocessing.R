#Libraries
library(readr)
library(dplyr)
library(BBmisc)

#Importing data
data <- read_csv("Dataset.csv")

#Round down values that have to be integers 
data$Households.surveyed = floor(data$Households.surveyed)
data$Women.age.15.49.years.interviewed = floor(data$Women.age.15.49.years.interviewed)
data$Men.age.15.49.years.interviewed = floor(data$Men.age.15.49.years.interviewed)


#Features required
features <- c(-4, -3, 12, 39, 48, 49, 50, 51, 52, 53, 57, 58, 61, 67, 68, 69, 70, 71, 76, 94)
features <- features+5
filtered_data = data %>%
  select(features)

#Removing NA values
sum(is.na(filtered_data[11]))
sum(is.na(filtered_data[12]))
sum(is.na(filtered_data[13]))
filtered_data = filtered_data %>%
  select(-c(11, 12, 13))

filtered_data_final = filtered_data[complete.cases(filtered_data), ]
sum(is.na(filtered_data_final))

data_final = filtered_data_final %>% 
  filter(Type=="Total")             

#Why Total?: For observations that have only rural or urban, total== rural or urban
#There might have a small no of observations in the other category so that they werent listed separately

#Normalising data
data_final_norm = normalize(data_final)

#Writing normalized data
write_csv(data_final_norm, "Dataset Cleaned.csv")