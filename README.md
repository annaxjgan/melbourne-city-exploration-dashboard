## Guide for running the interface

### Step 1: Install Required R Packages

Open R or RStudio and run the following command to install all necessary packages:

```R
install.packages(c(
  "shiny",     
  "leaflet",   
  "dplyr",     
  "tidyr",      
  "jsonlite",   
  "ggplot2",    
  "DT",         
  "shinyjs",    
  "geosphere",  
  "httr",       
  "lubridate"   
))
```

### Step 2: Run the Application

1. Open RStudio
2. Navigate to the project directory
3. Open `HomePage.R`
4. Click the "Run App" button in RStudio, or run the following command in the R console:

```R
source("HomePage.R")
```

