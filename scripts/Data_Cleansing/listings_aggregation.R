#Function to merge different listings
data_listings <- function(city_name) {
  # Define the directory path
  path <- paste0("C:/Users/matez/Desktop/Pulizia_dataset/", city_name)

  # Change the working directory
  setwd(path)
  
  # List all .csv files in the directory
  files_csv <- list.files(pattern = "listings_.*.csv")   
  
  # Import and merge all files
  listings_list <- lapply(files_csv, function(file) {
    read.csv(file, header = TRUE, sep = ",", quote = "\"", dec = ".", 
             fill = TRUE, comment.char = "", na.strings="")
  })
  
  # Combine all dataframes into one
  combined_data <- do.call(rbind, listings_list)   
  
  return(combined_data)
}



