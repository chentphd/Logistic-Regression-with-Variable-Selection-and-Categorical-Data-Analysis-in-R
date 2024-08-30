#Logistic Regression with Variable Selection and Categorical Data Analysis in R 
# Dataset of patients with heart failure 
# find and load dataset downloaded from 
# https://www.kaggle.com/andrewmvd/heart-failure-clinical-data

install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)

# Set your API credentials
kaggle_key <- Sys.getenv("71365bf364e5d32ccc7a450accb087a8")
kaggle_username <- Sys.getenv("chentphd")

# Define the API endpoint for the dataset
url <- "https://www.kaggle.com/api/v1/datasets/andrewmvd/heart-failure-clinical-data/download"

# Authenticate with Kaggle API and download the dataset
response <- GET(url, authenticate(kaggle_username, kaggle_key), write_disk("heart_failure_clinical_data.zip", overwrite = TRUE))

# Check if the download was successful
if (response$status_code == 200) {
  print("Dataset downloaded successfully.")
  
  # Unzip the downloaded file
  unzip("heart_failure_clinical_data.zip", exdir = "heart_failure_data")
  
  # List files in the directory
  list.files("heart_failure_data")
  
  # Read the CSV file into R
  heart_data <- read.csv("heart_failure_data/heart_failure_clinical_records_dataset.csv")
  
  # Preview the data
  head(heart_data)
  
} else {
  print(paste("Failed to download data. Status code:", response$status_code))
}
response$status_code


# Run Kaggle CLI to download the dataset
system("kaggle datasets download -d andrewmvd/heart-failure-clinical-data")

# Unzip the downloaded dataset
unzip("heart-failure-clinical-data.zip", exdir = "heart_failure_data")

# Load the CSV file into R
heart_data <- read.csv("heart_failure_data/heart_failure_clinical_records_dataset.csv")

# Preview the data
head(heart_data)

summary( heart_data )

#Step 1: Data Cleaning by setting categorical fields to factors and adjust other fields
heart_data$anaemia <- as.factor(heart_data$anaemia)
heart_data$sex <- as.factor(heart_data$sex)
heart_data$diabetes <- as.factor(heart_data$diabetes)
heart_data$high_blood_pressure <- as.factor( heart_data$high_blood_pressure)
heart_data$smoking <- as.factor(heart_data$smoking)
heart_data$DEATH_EVENT <- as.factor(heart_data$DEATH_EVENT)

#Adjust continuous fields for better interpretation 
heart_data$platelets <- heart_data$platelets/10000
heart_data$creatinine_phosphokinase <- scale(heart_data$creatinine_phosphokinase, scale = FALSE)


summary( heart_data )


#Cross tabulate select fields 
table1 <- table(heart_data$high_blood_pressure, heart_data$DEATH_EVENT )
table1
sum(table1)

#Run Chi-Sq test of independence 
chisq.test(table1)
chi1 <- chisq.test(table1)
#p-value = 0.2141 implies that the Null Hypothesis of
#high_blood_pressure and DEATH_EVENT are independent 


###############################################################################
# Create function to estimate the effect size for Chi-square test of 
# independence using Cramer's V. Unlike some other measures for effect
# size (e.g., Phi and Odds Ratio), Cramer's V can be used for tables 
# larger than 2x2 (Kim, 2017).

cramersv<-function(x,n,d){
  v<-sqrt(x/(n * d))
  return(v)
}
#x is chi-sq , n = total of all cells, d = df 

# Effect size interpretations for Cramer's V with df of 1:
#small is .1, medium is .3, and large is .5 

cramersv(1.5435, 299, 1 )
#[1] 0.0718485 ]
#Since it is less that .1, effect size is small 

###############################################################################
# Assumption

# The assumption for expected counts requires 80% of the cells to
# have an expected count of greater than 5 and no cell should have an expected
# count of less than one. 
chi1$expected
# 0        1
# 0 131.71237 62.28763
# 1  71.28763 33.71237
#All are greater than 5, so assumption is achieved 


# Note. If a violation is found you can set simulate.p.value = TRUE for 
# chisq.test()

###############################################################################

# If  you have a 2x2 table, you can possibly also use Fishers' 
# Exact Test. This test is generally more perferred for smaller 
# datasets.
fisher.test(table1, simulate.p.value = FALSE) 
#p-value = 0.1948

fisher.test(table1, simulate.p.value = TRUE) #Bigger thans 2x2 
###############################################################################
# Logistic Regression 
# Saturated model
mod1 <- glm(DEATH_EVENT ~ .
            , data = heart_data, family = "binomial")

summary(mod1)
#age                       0.0474191  0.0158006   3.001 0.002690 ** 
#ejection_fraction        -0.0766625  0.0163291  -4.695 2.67e-06 ***
#serum_creatinine          0.6660933  0.1814926   3.670 0.000242 ***
#time                     -0.0210446  0.0030144  -6.981 2.92e-12 ***


###############################################################################

# Run model selection method backward stepwise regression
# Starting from the saturated, a predictor is removed in each iteration,
# the predictor removed at each iteration is based on 
# then the AIC of the model is compared across all steps and the model with
# lowest AIC is selected.

# Use backwards step when number of predictors is not greater than
# sample size. Backwards step handles correlated predictors better.
# There is also Foward, Backwards and stepwise 
library(MASS)

mod_step <- stepAIC(mod1,
                    direction = 'backward', trace = FALSE)

mod_step
# Call:  glm(formula = DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + 
#              serum_sodium + time, family = "binomial", data = heart_data)

###############################################################################

# There are a number of issues with using stepwise regression
# (e.g., inflated R square)
# Therefore it may not necessarily lead to the best combination of
# predictors. In addition, it may lead to overfitting and results 
# may not be inconsistent.

# Use bootstrap resampling with replacement method to assess
# consistency predictors selected with stepwise
install.packages("bootStepAIC")
library(bootStepAIC)

mod_boot<-boot.stepAIC(mod1, heart_data, B = 50) #B = Replacement times 

mod_boot
#Covariates selected:Want 100%
#Coefficients Sign: Want One Sided, not 50/50
#Stat Significance:Want 100% 


#Final Model:
# DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + serum_sodium + 
#   time


###############################################################################
# Run logistic regression model with predictors recommended by step AIC.
mod2 <- glm(DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + 
              serum_sodium + time,
            data = heart_data, family = "binomial")

summary(mod2)




###############################################################################
# Return exponentiated coefficients to get the Odds Ratio
OR<-data.frame(exp(mod2$coefficients))
# Produce Percent Odds. 
OR_Perc<-(OR-1)*100
OR_Perc
#> OR_Perc
# exp.mod2.coefficients.
# (Intercept)                 1.326599e+06
# age                         4.338089e+00
# ejection_fraction          -7.079878e+00
# serum_creatinine            9.857373e+01
# serum_sodium               -6.251755e+00
# time                       -2.067808e+00

# For example, interpreting age:
# For every additional year older the odds of death increases by 4.3% while controlling 
# for all other predictors in the model.

# For every 10 year older the odds of death increases by 43% while controlling 
# for all other predictors in the model.


###############################################################################
# Use model to make predictions
# Create new dataset with one observation
newdata1 <- data.frame(age  = 61, ejection_fraction = 38, serum_creatinine=1.4 , 
                       serum_sodium=136, time=130)
# determine prob of Death
predict(mod2, newdata1, type="response")

# Create new dataset with one observation
newdata2 <- data.frame(age  = 95, ejection_fraction = 38, serum_creatinine=1.4 , 
                       serum_sodium=136, time=4)

# determine prob of Death
predict(mod2, newdata2, type="response")




###############################################################################


# Check Multicollinearity of predictor variables. Problem of 
# multicolinarity happens when two or more predictors that are highly correlated
# (i.e., linearly dependent). This may increase the standard error of the 
# coefficient and reduce the reliability of estimated coefficients.

# Independent variables with Variance Inflation Factor (VIF) values of 5 or 
# greater would suggest high correlation.
library(car)

vif(mod2)
# age ejection_fraction  serum_creatinine      serum_sodium              time 
# 1.053111          1.133484          1.079122          1.028355          1.096862 

#A VIF less than 5 indicates a low correlation of that predictor with other 
#predictors. A value between 5 and 10 indicates a moderate correlation, 
#while VIF values larger than 10 are a sign for high, not tolerable 
#correlation of model predictors