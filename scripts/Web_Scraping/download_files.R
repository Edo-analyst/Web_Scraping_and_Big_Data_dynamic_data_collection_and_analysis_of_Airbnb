library(stringr)
library(purrr)

# Function to download CSV files for a specific city
download_files <- function(city_name) {
  # Create a folder for the city
  dir.create(city_name, showWarnings = FALSE)
  
  # Find the <h3> element corresponding to the city
  city_h3 <- remDr$findElement(using = "xpath", sprintf("//h3[contains(text(),'%s')]", city_name))
  if (is.null(city_h3)) {
    stop("City not found on the page")
  }
  
  # Select all subsequent elements (following-sibling)
  city_elements <- city_h3$findChildElements(using = "xpath", "./following-sibling::*")
  
  # Iterate through subsequent elements
  for (el in city_elements) {
    tag_name <- el$getElementTagName()[[1]]  
    
    # Stop if another <h3> is encountered
    if (tag_name == "h3") {
      message("Encountered a new <h3>, stopping section processing for: ", city_name)
      break
    }
    
    # If it is an <h4>, process the date
    if (tag_name == "h4") {
      h4_text <- el$getElementText()[[1]]
      if (!grepl("[0-9]{2} [A-Za-z]+, [0-9]{4}", h4_text)) 
        next  # Skip invalid dates
      
      message("Valid date found: ", h4_text)
      
      # Find the table immediately following the <h4>
      table_elements <- el$findChildElements(using = "xpath", "./following-sibling::table[1]")
      if (length(table_elements) == 0) {
        message("No table found after <h4>: ", h4_text)
        next
      }
      
      table_element <- table_elements[[1]]
      
      # Download files from the links in the table
      file_types <- c("listings.csv.gz", "reviews.csv.gz","neighbourhoods.csv")
      for (file_type in file_types) {
        message("Looking for link: ", file_type)
        file_links <- table_element$findChildElements
        (using = "xpath",sprintf(".//a[contains(@href, '%s')]", file_type))
                                                      
        
        if (length(file_links) == 0) {
          message("No link found for: ", file_type)
          next
        }
        
        # Extract URL and date
        file_url <- file_links[[1]]$getElementAttribute("href")[[1]]
        message("Link found: ", file_url)
        
        # Attempt to extract the date from the URL
        file_date <- str_extract(file_url, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
        if (is.na(file_date)) {
          file_date <- gsub(", ", "_", h4_text)  
          file_date <- str_replace_all(file_date, " ", "_")
        }
        
        # Construct the destination file name, appending the date
        if (grepl("csv.gz", file_type)) {
          file_name <- str_replace(file_type, "\\.csv.gz", "")
          dest_file <- file.path(city_name, paste0(file_name, "_", file_date, ".csv.gz"))
        } else if (grepl("csv", file_type)) {
          file_name <- str_replace(file_type, "\\.csv", "")
          dest_file <- file.path(city_name, paste0(file_name, "_", file_date, ".csv"))
        }
        
        # Download the file
        message("Downloading: ", file_url, " -> ", dest_file)
        download.file(file_url, dest_file, mode = "wb")
      }
    }
  }

}  


