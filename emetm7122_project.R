# =======================================================================
# Analysis and Forecasting of the Philippine Consumer Price Index:
# An Econometric Approach Using RStudio
# =======================================================================

# Set working directory
getwd()
setwd("C:/Users/user/Documents/Wilson John/learning programming/R/emetm7122_project")

# Load necessary libraries
# install.packages(c("readr", "dplyr", "tseries", "forecast", "readxl"))
library(readr)        # Importing CSV files
library(dplyr)        
library(tseries)      # ADF, KSPSS, and PP test
library(forecast)     # TS model selection and forecasting/backcasting
library(readxl)       # Importing Excel


# Import Data
# Retrieved from 
# https://openstat.psa.gov.ph/PXWeb/pxweb/en/DB/DB__2M__PI__CPI__2018/0012M4ACP09.px/?rxid=8437fbf9-946c-4e5d-8426-0620f3a9c26a
# May 11, 2025
cpi <- read_excel("data/cpi_data_monthly_2018_2025april.xlsx")

# View the first few rows
head(cpi)

# Check for missing values (should be 0)
sum(is.na(cpi))

# Convert date column to Date type when needed
cpi$date <- as.Date(paste(cpi$year, cpi$month_n, "01", sep = "-"))
cpi$date_colname <- as.Date(cpi$date_colname, format = "%Y-%m-%d")

# Clean NA's
cpi <- cpi %>% 
  filter(!is.na(`date`))

# Summary statistics
summary(cpi[ , 5:21])

# Plot the cpi over time
plot(cpi$date, cpi$PHILIPPINES, type = "l",
     main = "Philippine CPI Jan 2018 - April 2025",
     xlab = "Date", ylab = "CPI - base year 2018")

# Plot ACF and PACF
Acf(cpi$PHILIPPINES, main = "ACF of Consumer Price Index")
Pacf(cpi$PHILIPPINES, main = "PACF of Consumer Price Index")

# Perform formal test (alpha = 0.05)
adf.test(cpi$PHILIPPINES)
kpss.test(cpi$PHILIPPINES)
pp.test(cpi$PHILIPPINES)

# If ADF and PP test (pval>alpha) and PP (pval<alpha)
diff_cpi <- diff(cpi$PHILIPPINES)
adf.test(diff_cpi)
kpss.test(diff_cpi)
pp.test(diff_cpi)

Acf(diff_cpi, main = "1st difference ACF of Consumer Price Index")
Pacf(cpi$PHILIPPINES, main = "1st difference PACF of Consumer Price Index")

# Plot differenced series
plot(cpi$date[-1], diff_cpi, type = "l",
     main = "Differenced CPI",
     xlab = "Date", ylab = "Differenced Consumer Price Index")

# Fit ARIMA model
arima_model <- auto.arima(cpi$PHILIPPINES)
summary(arima_model)

# Forecast next 12 months
forecast_values <- forecast(arima_model, h = 8)

# Plot forecast
plot(forecast_values, main = "CPI Forecast", xlim = c(0,20))
