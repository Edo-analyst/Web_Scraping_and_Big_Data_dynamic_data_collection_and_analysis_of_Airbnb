library(R.utils)

decompress_and_replace_gz <- function(city_name) {
  # Path to city directory
  directory <- file.path("C:/Desktop/Airbnb_Project", city_name)
  
  # List of .csv.gz files 
  gz_files <- list.files(directory, pattern = "\\.csv\\.gz$", full.names = TRUE)
  
  # Loop through each .csv.gz file
  for (gz_file in gz_files) {
    # Name of the file after decompression 
    decompressed_file <- sub("\\.csv\\.gz$", ".csv",
                             gz_file)
    
    # Decompress the .csv.gz file (with the .csv extension)
    gunzip(gz_file, destname = decompressed_file,
           overwrite = TRUE)
    
    # Print a message for each decompressed file
    cat("File decompressed:", gz_file, "to",
        decompressed_file, "\n")
  }
  
  # Completion message after processing all files for the city
  cat("Decompression completed for city:", city_name, "\n")
}

