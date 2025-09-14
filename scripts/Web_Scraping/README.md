# 🗂 Code Organization for Web Scraping
The entire scraping workflow is managed through the main script core_code.R.
This is the only file needed to run in order to perform the data collection.

🔸 core_code.R takes care of:
1) Starting and closing the Selenium session.
2) Navigating to the target website.
3) Iterating through the list of cities and triggering the scraping workflow.

🔸 Supporting scripts are automatically sourced within core_code.R:
- click_function.R: handles interaction with the webpage (scrolling and clicking buttons).
- download_files.R: downloads the dataset files for each city and organizes them.
- decompression.R: decompresses the downloaded archives and standardizes the resulting files (renaming and replacing where          needed).
