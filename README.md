

# Heart Failure Analysis - Logistic Regression with Variable Selection and Categorical Data Analysis in R

This project analyzes a dataset of heart failure patients using logistic regression in R, focusing on variable selection and handling categorical data.

## Dataset

The dataset is obtained from Kaggle: [Heart Failure Clinical Data](https://www.kaggle.com/andrewmvd/heart-failure-clinical-data).

## Installation and Setup

1. **Install Packages**
   ```r
   install.packages("httr")
   install.packages("jsonlite")
   install.packages("MASS")
   install.packages("bootStepAIC")
   install.packages("car")
   ```

2. **Load Libraries**
   ```r
   library(httr)
   library(jsonlite)
   library(MASS)
   library(bootStepAIC)
   library(car)
   ```

## Data Download and Preparation

1. **Authenticate and Download Dataset**
   ```r
   kaggle_key <- Sys.getenv("KAGGLE_KEY")
   kaggle_username <- Sys.getenv("KAGGLE_USERNAME")
   url <- "https://www.kaggle.com/api/v1/datasets/andrewmvd/heart-failure-clinical-data/download"
   response <- GET(url, authenticate(kaggle_username, kaggle_key), write_disk("heart_failure_clinical_data.zip", overwrite = TRUE))
   ```

2. **Unzip and Load Data**
   ```r
   unzip("heart_failure_clinical_data.zip", exdir = "heart_failure_data")
   heart_data <- read.csv("heart_failure_data/heart_failure_clinical_records_dataset.csv")
   ```

3. **Data Cleaning**
   - Convert categorical fields to factors.
   - Scale and adjust continuous fields.

## Analysis

1. **Exploratory Data Analysis**
   - Summary statistics and cross-tabulations.
   - Chi-square and Fisher's exact tests for independence.

2. **Logistic Regression**
   - Fit a saturated logistic regression model.
   - Use backward stepwise regression to select variables.
   - Bootstrap resampling to assess the consistency of selected predictors.

3. **Model Evaluation**
   - Obtain odds ratios and interpret coefficients.
   - Make predictions with the final model.

4. **Multicollinearity Check**
   - Use Variance Inflation Factor (VIF) to assess predictor correlation.

## Results

- The final logistic regression model includes predictors: age, ejection fraction, serum creatinine, serum sodium, and time.
- Significant predictors and their impact on the probability of death are analyzed.
- Predictions are made for new observations.

## References

- Kim, J. (2017). Cramer's V for Effect Size in Chi-Square Test of Independence.


