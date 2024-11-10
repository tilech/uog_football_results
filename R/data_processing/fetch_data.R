library(lubridate)
library(dplyr)

# Parameters
country <- "germany"
all_columns = FALSE

# File paths
input_path <- paste0("data/raw/", country)
output_path <- paste0("data/", country, if (all_columns) "_all_columns" else "", ".rds")

# Functions
combine_csv_files <- function(input_path, all_columns = TRUE) {
  # Get the list of all CSV files in the directory
  csv_files <- list.files(path = input_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Read each CSV file into a list of dataframes
  data_list <- lapply(csv_files, read.csv, stringsAsFactors = FALSE)
  
  if (all_columns) {
    # Combine all dataframes and fill missing columns with NA
    combined_data <- bind_rows(data_list)
  } else {
    # Find the common columns across all dataframes
    common_columns <- Reduce(intersect, lapply(data_list, colnames))
    
    # Filter each dataframe to keep only the common columns
    filtered_data_list <- lapply(data_list, function(df) df[, common_columns, drop = FALSE])
    
    # Bind the filtered dataframes together
    combined_data <- do.call(rbind, filtered_data_list)
  }
  
  # Add the country
  combined_data$Country <- country
  
  # Change date format
  combined_data$Date <- dmy(combined_data$Date)
  
  return(combined_data)
}

combined_data <- combine_csv_files(input_path, all_columns = all_columns)

# Save the combined data frame as an RDS file
saveRDS(combined_data, file = output_path)

# Return a confirmation message
message("Combined data has been saved to ", output_path)
