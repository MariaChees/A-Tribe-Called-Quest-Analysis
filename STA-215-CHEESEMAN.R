## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: STA215-CHEESEMAN.R
# Date:      2023_11_20
# Who:       Maria A. Cheeseman



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
# EXAMINE Race_Ethnicity
mean(data$Race_Ethnicity)
sd(data$Race_Ethnicity)
table(data$Race_Ethnicity)
describe(data$Race_Ethnicity)
summary(data$Race_Ethnicity)

# EXAMINE Poverty_Class
mean(data$Poverty_Class)
sd(data$Poverty_Class)
table(data$Poverty_Class)
describe(data$Poverty_Class)
summary(data$Poverty_Class)

# EXAMINE Instrumentals 
mean(data$Instrumentals)
sd(data$Instrumentals)
table(data$Instrumentals)
describe(data$Instrumentals)
summary(data$Instrumentals)

# EXAMINE Lyrics
mean(data$Lyrics)
sd(data$Lyrics)
table(data$Lyrics)
describe(data$Lyrics)
summary(data$Lyrics)


##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
# Use the table() command with two variables
# The first variable should be the rows and the second should be the column
table(data$Instrumentals, data$Lyrics)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
# Use the chisq.test() command with two variables in a contingency table
chisq.test(table(data$Instrumentals, data$Lyrics))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
# Use the aov() command with a qualitative variable and a quantitative variable
aov(Race_Ethnicity ~ Lyrics, data = data)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
# Calculate the correlation between race/ethnicity and poverty/class "cor()"
cor(data$Race_Ethnicity , data$Poverty_Class)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
# Calculate linear regression line (i.e., slope)
linear_relationship <- lm(Race_Ethnicity ~ Poverty_Class, data = data)
summary (linear_relationship)
##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
# Examine the scatter plot showing the relationship between race/ethnicity and poverty/class
linear_plot <- plot(data$Race_Ethnicity, data$Poverty_Class)
print(linear_plot)

# Add the linear regression line to the scatter plot
abline(linear_relationship, col = "red")

# Add a line for the mean of poverty/class
abline(h = 0.65, col = "blue")

# Add a line for the mean of race/ethnicity
abline(v=1.68, col = "green")
##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
# Plot the residuals
plot(data$Race_Ethnicity, residuals(linear_relationship))

# Add a line for the mean residual value of poverty/class
abline(h = 0.65, col = "red")


