############### MASTER SCRIPT FOR R ON MASTER THESIS ##############
install.packages("groundhog")
library("groundhog")
pkgs <- c("tidyverse", # For data manipulation and basic ggplot2 visualisation
          "WDI", # For loading data from the World Bank Development Indicator 
          "plm", # The package contains linear panel models for FE or RE
          "countrycode", # For iso3c codes 
          "plotly", # Useful for interractive chart
          "gridExtra", # For plotting multiple panel of cross-sectional data
          "missForest", # For missing values imputation
          "jasonlite", # importing data from UN statistics
          "httr", # Helper package for jasonlite
          "sf", # Geographical mapping
          "foreign", # Tyding data and exporting data in table and latex
          "mediation", # Regression model package
)

groundhog.library(pkgs, today()) # Today helper function ensures the packages are automatically up todate

