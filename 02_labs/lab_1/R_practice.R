# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Lab Session: Data Import, Cleaning, and Regression Analysis in R
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: Matias Villalba
# Date: 16/10/2024
# Purpose: This lab session aims to provide hands-on practice with importing various types of data files into R, performing essential data cleaning tasks, and conducting regression analysis.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# R Practice ###################################################################


## Importing data on R =========================================================

rm(list=ls())

### CSV Files ------------------------------------------------------------------

csv_file <- url("https://raw.githubusercontent.com/mvillalbao/GMU_Experimental/refs/heads/main/01_data/raw_dataset.csv") # replace with path/to/csv_file.csv

data_csv <- read.csv(csv_file)

### Excel Files ----------------------------------------------------------------

xlsx_file <- "https://raw.githubusercontent.com/mvillalbao/GMU_Experimental/refs/heads/main/01_data/raw_dataset.xlsx" # replace with path/to/xlsx_file.xlsx

library(openxlsx)
data_xlsx <- read.xlsx(xlsx_file, sheet = "Sheet1")

### Stata Files ----------------------------------------------------------------

dta_file <- url("https://raw.githubusercontent.com/mvillalbao/GMU_Experimental/refs/heads/main/01_data/raw_dataset.dta") # replace with path/to/dta_file.dta

library(haven)
data_dta <- read_dta(dta_file)

### Text Files -----------------------------------------------------------------

txt_file <- url("https://raw.githubusercontent.com/mvillalbao/GMU_Experimental/refs/heads/main/01_data/raw_dataset.txt") # replace with path/to/txt_file.txt

data_txt <- read.table(txt_file, header = TRUE, sep = "\t")

### SPSS Files -----------------------------------------------------------------

sav_file <- url("https://raw.githubusercontent.com/mvillalbao/GMU_Experimental/refs/heads/main/01_data/raw_dataset.sav") # replace with path/to/sav_file.sav

data_sav <- read_sav(sav_file)



## Data Cleaning Practice ======================================================

csv_file <- url("https://raw.githubusercontent.com/mvillalbao/GMU_Experimental/refs/heads/main/01_data/raw_dataset.csv") # replace with path/to/csv_file.csv

data <- read.csv(csv_file)
head(data)

# In R, you can generate new variables by simply assigning them to your dataset.
# Let's say we want to create a new variable 'total_sales' as the sum of 'product_A' and 'product_B'
data$product_A <- as.numeric(data$product_A) # First we coerce 'product_A' to numeric
data$total_sales <- rowSums(data[, c("product_A", "product_B")], na.rm = TRUE)

# The 'price' column contains commas instead of points for decimals.
data$price <- as.numeric(gsub(",", ".", data$price))

# The 'growth_rate' string column contains percentages (e.g., "50%").
data$growth_rate <- as.numeric(gsub("%", "", data$growth_rate))/100

# Clean 'customer_name' and 'region' column: remove leading/trailing spaces and standardize to lowercase.
data$customer_name <- tolower(trimws(data$customer_name))
data$region <- tolower(trimws(data$region))

# For 'sales_target', convert "unavailable" to NA and then convert the column to numeric.
data$sales_target[data$sales_target == "unavailable"] <- NA
data$sales_target <- as.numeric(data$sales_target)


# Regression Analysis Practical Application ####################################

## Setup =======================================================================

library(readr)

urlfile = "https://raw.githubusercontent.com/mvillalbao/CausalInferenceML/refs/heads/main/data/wage2015_subsample_inference.csv"

data <- read_csv(url(urlfile))

## Data Cleaning ===============================================================

# Create the variable 'high_wage'
data$high_wage <- ifelse(data$wage > mean(data$wage), 1, 0)

# Create the variable 'college_or_above'
data$college_or_above <- ifelse(data$clg == 1 | data$ad == 1, 1, 0)

# Create the variable based on the highest level of education attained
data$education <- factor(
  ifelse(data$ad == 1, "AD",       # Advanced Degree
      ifelse(data$clg == 1, "CLG",  # College Degree
          ifelse(data$scl == 1, "SCL",  # Some College
              ifelse(data$hsg == 1, "HSG",  # High School Grad
                  ifelse(data$shs == 1, "SHS",  # Some High School
                      "NONE") # No education
                  )
              )
          )
      ),
  levels = c("NONE", "SHS", "HSG", "SCL", "CLG", "AD")
  )

## Data Analysis ===============================================================

### Model 1: Continuous Y and continuous X with intercept ----------------------
model1 <- lm(wage ~ exp2, data = data)
summary(model1)


# Extract the coefficients from the model
coef_model1 <- summary(model1)$coefficients

# Extract the specific coefficient
beta_1 <- coef_model1["exp2", "Estimate"]
beta_0 <- coef_model1["(Intercept)", "Estimate"]

cat("The coefficient on experience (exp2) is", round(beta_1, 2),
    ", which indicates that for every additional year of experience, the wage is predicted to increase by approximately",
    round(beta_1, 2), "units, holding all other factors constant.\n")

cat("The intercept is", round(beta_0, 2),
    ", which means that the expected wage for someone with zero years of experience is",
    round(beta_0, 2), "units.\n")

### Model 2: Continuous Y and binary/dummy X with intercept---------------------

data$sex <- as.factor(data$sex)
model2 <- lm(wage ~ sex, data = data)
summary(model2)

# Extract the coefficients from the model
coef_model2 <- summary(model2)$coefficients

# Extract the specific coefficient
beta_1_m2 <- coef_model2["sex1", "Estimate"]
beta_0_m2 <- coef_model2["(Intercept)", "Estimate"]

cat("The coefficient on the gender dummy variable (sex) is", round(beta_1_m2, 2), 
    ", which indicates that females (sex = 1) are predicted to earn approximately", 
    round(beta_1_m2, 2), "units less than males (the reference group, sex = 0).\n")

cat("The intercept is", round(beta_0_m2, 2), 
    ", which means that the expected wage for males (sex = 0) is", 
    round(beta_0_m2, 2), "units.\n")

### Model 3: Continuous Y and binary/dummy X without intercept------------------

model3 <- lm(wage ~ sex - 1, data = data) # Removing the intercept
summary(model3)

# Extract the coefficients from the model (no intercept)
coef_model3 <- summary(model3)$coefficients

# Extract the specific coefficients
beta_female_m3 <- coef_model3["sex1", "Estimate"]
beta_male_m3 <- coef_model3["sex0", "Estimate"]

cat("The coefficient for females (sex = 1) is", round(beta_female_m3, 2), 
    ", which indicates that the expected wage for female is", round(beta_female_m3, 2), "units.\n")

cat("The coefficient for males (sex = 0) is", round(beta_male_m3, 2), 
    ", which indicates that the expected wage for males is", round(beta_male_m3, 2), "units.\n")

### Model 4: Binary/dummy Y and Continuous X with intercept --------------------

model4 <- lm(high_wage ~ exp2, data = data)
summary(model4)

# Extract the coefficients from the linear probability model (LPM)
coef_model4 <- summary(model4)$coefficients

# Extract the specific coefficients for intercept and experience
beta_0_m4 <- coef_model4["(Intercept)", "Estimate"]
beta_1_m4 <- coef_model4["exp2", "Estimate"]

cat("The intercept is", round(beta_0_m4, 2), 
    ", which indicates that the predicted probability of earning a high wage for someone with 0 years of experience is", 
    round(beta_0_m4, 2), ".\n")

cat("The coefficient on experience is", round(beta_1_m4, 4), 
    ", which indicates that for each additional year of experience, the probability of earning a high wage is predicted to increase", 
    round(beta_1_m4 * 100, 2), "percentage points.\n")

### Model 5: Binary/dummy Y and Binary/dummy X with intercept ------------------

model5 <- lm(high_wage ~ college_or_above, data = data)
summary(model5)

# Extract the coefficients from the linear probability model (LPM)
coef_model5 <- summary(model5)$coefficients

# Extract the specific coefficients for intercept and college_or_above
beta_0_m5 <- coef_model5["(Intercept)", "Estimate"]
beta_1_m5 <- coef_model5["college_or_above", "Estimate"]

cat("The intercept is", round(beta_0_m5, 2), 
    ", which indicates that the predicted probability of earning a high wage for someone without a college degree is", 
    round(beta_0_m5 * 100, 2), "percentage points.\n")

cat("The coefficient on college_or_above is", round(beta_1_m5, 4), 
    ", which indicates that having a college degree (or higher) is predicted to increase the probability of earning a high wage by", 
    round(beta_1_m5 * 100, 2), "percentage points compared to those without a college degree.\n")

### Model 6: Continuous Y and categorical X with intercept ---------------------

table(data$education)

model6 <- lm(wage ~ education, data = data)

# Display the summary of the model
summary(model6)

# Extract the coefficients from the linear regression model
coef_model6 <- summary(model6)$coefficients

# Extract the specific coefficients for intercept and education levels
beta_0_m6 <- coef_model6["(Intercept)", "Estimate"]
beta_hsg_m6 <- coef_model6["educationHSG", "Estimate"]
beta_scl_m6 <- coef_model6["educationSCL", "Estimate"]
beta_clg_m6 <- coef_model6["educationCLG", "Estimate"]
beta_ad_m6 <- coef_model6["educationAD", "Estimate"]

cat("The intercept is", round(beta_0_m6, 2), 
    ", which indicates that the predicted average wage for individuals in the reference category (Some High School) is", 
    round(beta_0_m6, 2), "units.\n")

cat("The coefficient on High School Graduate is", round(beta_hsg_m6, 2), 
    ", which indicates that individuals with a High School Diploma are predicted to earn", 
    round(beta_hsg_m6, 2), "units less compared to individuals with Some High School education.\n")

cat("The coefficient on Some College is", round(beta_scl_m6, 2), 
    ", which indicates that individuals with Some College are predicted to earn", 
    round(beta_scl_m6, 2), "units less compared to individuals with Some High School education.\n")

cat("The coefficient on College Graduate is", round(beta_clg_m6, 2), 
    ", which indicates that individuals with a College Degree are predicted to earn", 
    round(beta_clg_m6, 2), "units more compared to individuals with Some High School education.\n")

cat("The coefficient on Advanced Degree is", round(beta_ad_m6, 2), 
    ", which indicates that individuals with an Advanced Degree are predicted to earn", 
    round(beta_ad_m6, 2), "units more compared to individuals with Some High School education.\n")


## Result table with "stargazer" Package =======================================

library(stargazer)
library(rstudioapi) # To display table on RStudio directly

temp_file <- tempfile(fileext = ".html")

stargazer(model1, model2, model3, model4, model5, model6,
          type = "text",  # Use "text" for console output, "html" or "latex"
          title = "Model Estimation Results",
          align = TRUE,
          column.labels = c("Model 1",
                            "Model 2",
                            "Model 3",
                            "Model 4",
                            "Model 5",
                            "Model 6"),
          covariate.labels = c("Experience",
                               "Sex (0=Male)",
                               "Sex (1=Female)",
                               "College or Above",
                               "Education (High School Graduate)",
                               "Education (Some College)",
                               "Education (College Graduate)",
                               "Education (Advanced Degree)"),
          out = temp_file) # could export as Latex, text, and html.

viewer(temp_file)

## Result table with "sjPlot" Package ==========================================

library(sjPlot)

# Create a regression table for all models
tab_model(model1, model2, model3, model4, model5, model6,
          title = "Regression Results",
          dv.labels = c("Wage (Continuous Y, Continuous X)",
                        "Wage (Continuous Y, Dummy X)",
                        "Wage (Continuous Y, Dummy X, No Intercept)",
                        "High Wage (Dummy Y, Continuous X)",
                        "High Wage (Dummy Y, Dummy X)",
                        "Wage (Continuous Y, Categorical X)"))


## Plotting Coefs. Across Models ===============================================

library(ggplot2)
library(broom)

model1_tidy <- tidy(model1, conf.int = TRUE)
model2_tidy <- tidy(model2, conf.int = TRUE)
model3_tidy <- tidy(model3, conf.int = TRUE)
model4_tidy <- tidy(model4, conf.int = TRUE)
model5_tidy <- tidy(model5, conf.int = TRUE)
model6_tidy <- tidy(model6, conf.int = TRUE)

model1_tidy$model <- "Model 1"
model2_tidy$model <- "Model 2"
model3_tidy$model <- "Model 3"
model4_tidy$model <- "Model 4"
model5_tidy$model <- "Model 5"
model6_tidy$model <- "Model 6"

all_models_tidy <- rbind(model1_tidy, model2_tidy, model3_tidy, model4_tidy, model5_tidy, model6_tidy)

ggplot(all_models_tidy, aes(x = term, y = estimate, color = model)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point(size = 2, shape = 21, fill = "white") +
  facet_wrap(~model, scales = "free_x", nrow = 1) +
  theme_minimal() +
  labs(title = "Coefficient Estimates Across Models",
       x = "Terms",
       y = "Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12))
