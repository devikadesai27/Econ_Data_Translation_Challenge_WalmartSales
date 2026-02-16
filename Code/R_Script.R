# Install packages
install.packages("lubridate")

# load library
library(tidyverse)
library(fixest)
library(lubridate)

# Load Walmart_Sales.csv file
walmart_sales <- read_csv("./raw_data/Walmart_Sales.csv")

# Data Cleaning
# Check for missing values
colSums(is.na(walmart_sales))

# Convert Date column into proper Date format
# Use dmy because format is YYYY-MM-DD
walmart_sales <- walmart_sales %>%
  mutate(Date = dmy(Date))

# Create month and year variables 
# Month fixed effects control for seasonality (temperature and sales both vary by season)
walmart_sales <- walmart_sales %>%
  mutate(month = month(Date),
         year = year(Date))

# Exploratory visualization
# Plot Temperature and Weekly sales to visually inspect whether the relationship appears linear or non-linear.
ggplot (walmart_sales, aes(x = Temperature, y= Weekly_Sales))+
  geom_point(alpha=0.2)+
  geom_smooth()

# Key variables for regression
# create a squared term to allow for non linearity 
# Log-transform sales so coefficients can be interpreted as percentage changes and to reduce heteroskedasticity.
walmart_sales <- walmart_sales %>%
  mutate(
    Temp_sq = Temperature^2,
    log_sales = log(Weekly_Sales)
    )

# Panel fixed effects regression
# Model specification: log(Sales) = β0 + β1*Temp + β2*Temp^2 + controls + FE + error
# controls: Holiday_Flag (holiday demand shocks), Fuel_Price (transport costs affecting shopping behavior), CPI (inflation), Unemployment(local economic conditions)
# Fixed effects:
# Store FE: controls for time-invariant differences across stores (size, location, demographics)
# Month FE: controls for seasonal patterns 
# Year FE: controls for time trends
# Clustered standard errors at the store level account for serial correlation within stores over time.
model1 <- feols(log_sales ~ Temperature + Temp_sq +
                  Holiday_Flag + Fuel_Price + CPI + Unemployment 
                   | Store + month + year, 
                data = walmart_sales,
                vcov = ~ Store)

# Display regression table for model1
etable(model1)

# Check for heteroskedasticity 
walmart_sales <- walmart_sales %>%
  mutate(
    fitted = fitted(model1),
    residuals= resid(model1)
  )

# Plot Residual vs Fitted 
ggplot(walmart_sales, aes(fitted, residuals)) +
  geom_point(alpha =0.3, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
  labs(title= "Residual vs Fitted", x= "Fitted", 
       y= "Residuals") +
  theme_minimal()

# Model 2: Excludes Unemployment to test robustness.
# If temperature coefficients remain similar, results are robust
# Include store, month, year fixed effects.
# Clustered standard errors at store level
model2 <- feols(log_sales ~ Temperature + Temp_sq +
                  Holiday_Flag + Fuel_Price + CPI
                | Store + month + year,
                data= walmart_sales,
                vcov = ~ Store)

# Display regression table for model 2
etable(model2)



