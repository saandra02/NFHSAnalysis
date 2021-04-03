library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(gdata)
library(janitor)
library(stringr)


#Getting all the files
files <- (Sys.glob("*.csv"))

#Working with the first file
file <- files[1]
unit_data <- read_csv(file)
column_names <- colnames(unit_data)
req_cols = ncol(unit_data)

#Remove the Notes about each unit columns
for (i in 1:length(column_names)) {
  if(startsWith(column_names[i], "Note of", trim = TRUE)){
    req_cols = i-1
    break
  }
}
unit_data_trim = unit_data[,1:req_cols]


unit_data_transpose = transpose(unit_data_trim) #Features become columns and districts become rows
colnames(unit_data_transpose) <- rownames(unit_data_trim)
rownames(unit_data_transpose) <- colnames(unit_data_trim)
unit_data_transpose = row_to_names(unit_data_transpose, row_number = 1)

#Adding features
res <- str_match(file, "-\\s*(.*?)\\s*_")
unit_state <- res[,2]
unit_data_transpose$state = unit_state

#Creating the base of the final dataset
data = unit_data_transpose

#Adding the other units to the dataset
for(i in 2:length(files)){
  file <- files[i]
  unit_data <- read_csv(file)
  column_names <- colnames(unit_data)
  req_cols = ncol(unit_data)
  
  #Remove the Notes about each unit columns
  for (j in 1:length(column_names)) {
    if(startsWith(column_names[j], "Note of", trim = TRUE)){
      req_cols = j-1
      break
    }
  }
  unit_data_trim = unit_data[,1:req_cols]
  
  
  unit_data_transpose = transpose(unit_data_trim) #Features become columns and districts become rows
  colnames(unit_data_transpose) <- rownames(unit_data_trim)
  rownames(unit_data_transpose) <- colnames(unit_data_trim)
  unit_data_transpose = row_to_names(unit_data_transpose, row_number = 1)
  
  #Adding features
  res <- str_match(file, "-\\s*(.*?)\\s*_")
  unit_state <- res[,2]
  if(unit_state=='J'){
    unit_state = "J_and_K"
  }
  unit_data_transpose$state = unit_state
  
  data = rbind(data, setNames(unit_data_transpose, names(data)))
  
}

#Verify all the files have been added
length(unique(data$state)) #should be 30 if merge was done for all files

#Split rural-urban-total into a new column
data <- cbind(rownames(data), data.frame(data, row.names=NULL))
data = separate(data = data, col = `rownames(data)`, into = c("District", "Type"), sep = "-")
#Fix for districts that had a hyphen in their name
data[222, 1] = "Janjgir-Champa"
data[222, 2] = "Rural"
data[223, 1] = "Janjgir-Champa"
data[223, 2] = "Total"

#Rural, Urban and total would be a double count of datapoints that would included in both rural/urban and in total 
#Remove one set either rural/urban or total depending on purpose of study

#Write data into new file
write_csv(data, "Dataset.csv")
