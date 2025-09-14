# Load required library
library(RSelenium)

# Open Selenium
rD <- rsDriver(browser="firefox", port=4555L, verbose=F, 
               phantomver = NULL, chromever = NULL)
remDr <- rD$client
#remDr$open()

# Navigate to the target URL
url <- "http://insideairbnb.com/get-the-data.html"
remDr$navigate(url)


##Italt elements
city_section <- remDr$findElements(
  using = 'xpath', '//h3[contains(text(), "Italy")]')

# Extract text
city_title <- sapply(city_section, 
                     function(el) el$getElementText())


# Cities names
city_name <- sapply(city_title,
                    function(x) strsplit(x, ",")[[1]][1])  


source("click_function.R")   
source("download_files.R")   
source("decompression.R")    

for(i in 1 : length(city_name)){
  button_click(city_name[i])  
  download_files(city_name[i]) 
  decompress_and_replace_gz(city_name[i])
}

# Close Selenium
remDr$close()
