button_click <- function(city_name){
  
  # Construct the dynamic XPath with the provided city name
  xpath_button <- sprintf('//a[@class="showArchivedData" 
                          and @data-city="%s"]', city_name)
  
  
  # Find show button for city
  button <- remDr$findElement(using = 'xpath', xpath_button)
  
  # Use JavaScript to scroll to the button with a safe margin
  remDr$executeScript("arguments[0].scrollIntoView({behavior: 
                      'smooth', block: 'center'});", list(button))
  
  # Add delay to let the page scroll and stabilize (0.5 sec)
  Sys.sleep(0.5)  
  
  # Check if the button is visible
  button_style <- button$getElementAttribute("style")[[1]]
  is_visible <- !(grepl("display: none", button_style) || 
                    grepl("visibility: hidden", button_style))
  
  # Check if the button is disabled (if "disabled" attribute exists)
  is_enabled <- TRUE
  tryCatch({
    is_enabled <- button$getElementAttribute("disabled")[[1]] != "true"
  }, error = function(e) {
    # If not, the button is clickable
    is_enabled <- TRUE
  })
  
  if (is_visible && is_enabled) {
    message("The button is visible and enabled. Clicking now.")
    
    # Force click with JavaScript if normal click doesn't work
    tryCatch({
      # Traditional click
      button$clickElement()
    }, error = function(e) {
      message("Traditional click did not work. Trying JavaScript click.")
      # Use JavaScript to force the click
      remDr$executeScript("arguments[0].click();", list(button))
    })
    
    message("Button clicked successfully!")
  } else {
    message("The button is not visible or enabled.")
  }
}
