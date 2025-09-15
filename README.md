# 🏡 Web Scraping and Big Data dynamic data collection and analysis of Airbnb

## 📖 Overview
Analysis and extraction of data from dynamic web pages using the RSelenium library to gather information from Airbnb listings related to Italy. Subsequently, machine learning methods are applied to compare differences between cities and identify the main factors influencing rental prices in different areas. 

## 📂 Data Collection 
The datasets used in this project were collected from [InsideAirbnb](https://insideairbnb.com/), which provides publicly available data on Airbnb listings.
The entire process was **automated** using **RSelenium** in R, enabling programmatic browser interaction.

### 🔹 Collection process

1. **Environment setup**

   * Installed Java, Selenium Server, and GeckoDriver (required for Firefox).
   * Launched a Firefox instance via `rsDriver()` in RSelenium.

3. **Navigation & city identification**

   * Accessed the *Get the Data* section of InsideAirbnb.
   * Extracted `<h3>` elements containing the string *Italy* using XPath queries to retrieve the list of Italian cities.

4. **Accessing hidden data**

   * Some tables are only available after interacting with hidden buttons (*showArchivedData*).
   * A custom R function was implemented to automatically click these buttons, making archived datasets accessible.

5. **Dataset download**

   * Created a dedicated folder for each city.
   * Located download links (`.csv` and `.csv.gz`) using XPath, and saved them with filenames that include the corresponding reference date.

6. **Decompression of compressed files**

   * Files in `.csv.gz` format were automatically decompressed with `gunzip()` (from the **R.utils** package) and replaced with their `.csv` versions.

---

### 🔹 Types of collected data

The downloaded datasets from InsideAirbnb include:

* **`listings.csv`** → detailed information on listings (price, property type, location, host, amenities, etc.).
* **`reviews.csv.gz`** → full guest reviews (date, text, listing ID).
* **`neighbourhoods.csv`** → list of neighborhoods in each city.
* **`neighbourhoods.geojson`** → geographic boundaries of neighborhoods, useful for spatial analysis.

---

👉 This automated pipeline ensures a **structured, reproducible, and up-to-date data collection process**, enabling systematic analysis of Airbnb activity across multiple Italian cities.

---


## 📊 Data Cleansing

## 📈 Exploratory Data Analysis

## 🤖 Modeling

## 📌 Results
