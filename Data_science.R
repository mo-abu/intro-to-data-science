library(tidyverse)
# library(dplyr)
library(lessR)
library(janitor)

### Part 1. Wrangling data ####
# Load data set
wd <- "C:/Users/habba/OneDrive/Documents/Introduction to Data science (R)/20109816_assessment_2/Df"
setwd(wd)
data <- read.csv("accidents.csv")
View(data)

# summary
str(data)
str(data$X1st.Road.Class)
summary(data)
sapply(data, typeof)

# size
dim(data)
nrow(data)
ncol(data)
names(data) 

# Missing value in column
any(is.na(data))
sum(is.na(data)) 
colSums(is.na(data))

# select all rows where a variable is none and record as NA
missing <- data[!complete.cases(data),]
missing

# Check for inconsistency 
# Shows all the unique values in Road Surface
unique(data$Road.Surface)

# Changing surfaces to its matching number from "Guidance"
data <- data %>% mutate(Road.Surface = sub("Wet/Damp", "2", Road.Surface)) %>%
      mutate(Road.Surface = sub("Wet ¨ Damp", "2", Road.Surface)) %>%  
      mutate(Road.Surface = sub("Wet", "2", Road.Surface)) %>% 
      mutate(Road.Surface = sub("Snow", "3", Road.Surface)) %>% 
      mutate(Road.Surface = sub("Frost/Ice", "4", Road.Surface)) %>% 
      mutate(Road.Surface = sub("Ice", "4", Road.Surface)) %>% 
      mutate(Road.Surface = sub("Dry", "1", Road.Surface)) 

# Checking if the anomalies are fixed
unique(data$Road.Surface)
data

# Check for Unique 1st.Road.Class
unique(data$X1st.Road.Class)
# Changing 1st.Road.Class to its matching number from "Guidance" USING EXPRESSION LANGUAGE 
data <- data %>% mutate(X1st.Road.Class = sub("M\\d{1,9}", "1", X1st.Road.Class)) %>% 
      mutate(X1st.Road.Class = sub("A\\d{1,9}\\(M)", "2", X1st.Road.Class)) %>% 
      mutate(X1st.Road.Class = sub("A\\d{1,9}", "3", X1st.Road.Class)) %>% 
      mutate(X1st.Road.Class = sub("B\\d{1,9}", "4", X1st.Road.Class)) %>%
      mutate(X1st.Road.Class = sub("U", "6", X1st.Road.Class))

# Checking if the anomalies are fixed
unique(data$X1st.Road.Class)
data

# Unnecessarily columns
str(data)
updated <- data%>%select(-c("Daylight.Dark","Local.Authority"))
updated

# Outliers 

# 3sigma Rule
# Standard deviation for age of casualty
sd_value <- sd(updated$Age.of.Casualty, na.rm = "TRUE") # na.rm skips NA value
sd_value
# Mean for age of casualty
mean_Value <- mean(updated$Age.of.Casualty, na.rm = "TRUE")
mean_Value
# calculate upper and lower bounds
upper_bound <- mean_Value + 3*sd_value
lower_bound <- mean_Value - 3*sd_value
upper_bound
lower_bound
# Extract outliers
outliers_sigma <-updated %>% filter((Age.of.Casualty > upper_bound)| (Age.of.Casualty < lower_bound))
outliers_sigma

# Hampel Identifier
# Calculate median and MAD
median_value <- median(updated$Age.of.Casualty, na.rm = "TRUE")
MAD_value <- mad (updated$Age.of.Casualty, na.rm = "TRUE")
median_value
MAD_value
# Calculate upper and lower bounds
upper_bound <- median_value + 3*MAD_value
lower_bound <- median_value - 3*MAD_value
upper_bound
lower_bound
# Extract outliers found be Hampel identifier
outliers_hampel <- updated %>% filter((Age.of.Casualty > upper_bound)| (Age.of.Casualty < lower_bound))
outliers_hampel 

# Boxplot 
boxplot(updated$Age.of.Casualty)
outlier <- boxplot(updated$Age.of.Casualty, plot=TRUE)$out
outlier

# Removing outliers from the dataset
# Rows with outliers  
updated[which(updated$Age.of.Casualty %in% outlier),]

# Rows containing the outliers
clean <- updated[-which(updated$ Age.of.Casualty %in% outlier),]
# Checking boxplot to see if outliers are gone
boxplot(clean$Age.of.Casualty)
View(clean)

# save
write.csv(clean, "clean_accident.csv")


### Part 2. Exploration ####
clean
names(clean)

# Comparing male & female casualties with weather Conditions
# Filter Weather.Conditions and Sex.of.Casualty
wsdata <- clean[,c("Weather.Conditions", "Sex.of.Casualty")]

# Number of male & female casualties in each weather conditions and their %
# Table showing Number of Accidents caused by male(1) & female(2) in each weather conditions
table(wsdata$Sex.of.Casualty, wsdata$Weather.Conditions)
# % of male & female casualties in each weather conditions 
BarChart(data = clean, Weather.Conditions, by = Sex.of.Casualty,
         main = "% of male & female casulties in each weather conditions",
         legend_labels = c("male", "female"), 
         xlab = "Weather conditions", ylab = "Number of accidents")

# Compare the rate of accidents by each sex in various weather conditions
# Weather conditions: (1) Fine without high winds
male <- count(subset(wsdata, Sex.of.Casualty == 1 & Weather.Conditions == 1))
male
female <- count(subset(wsdata, Sex.of.Casualty == 2 & Weather.Conditions == 1))
female
difference <- as.integer( male - female)
difference # male 407 more

# Weather conditions: (2) Raining without high winds
male <- count(subset(wsdata, Sex.of.Casualty == 1 & Weather.Conditions == 2))
male
female <- count(subset(wsdata, Sex.of.Casualty == 2 & Weather.Conditions == 2))
female
difference <- as.integer( male - female)
difference # male 22 more

# Weather conditions: (3) Snowing without high winds
male <- count(subset(wsdata, Sex.of.Casualty == 1 & Weather.Conditions == 3))
male
female <- count(subset(wsdata, Sex.of.Casualty == 2 & Weather.Conditions == 3))
female
difference <- as.integer( male - female)
difference # male 2 more

# Weather conditions: (4) Fine with high winds
male <- count(subset(wsdata, Sex.of.Casualty == 1 & Weather.Conditions == 4))
male
female <- count(subset(wsdata, Sex.of.Casualty == 2 & Weather.Conditions == 4))
female
difference <- as.integer( male - female)
difference # male 1 more

# Weather conditions: (5) Raining with high winds
male <- count(subset(wsdata, Sex.of.Casualty == 1 & Weather.Conditions == 5))
male
female <- count(subset(wsdata, Sex.of.Casualty == 2 & Weather.Conditions == 5))
female
difference <- as.integer( male - female)
difference # male 11 more

# Weather conditions: (6) Snowing with high winds
male <- count(subset(wsdata, Sex.of.Casualty == 1 & Weather.Conditions == 6))
male
female <- count(subset(wsdata, Sex.of.Casualty == 2 & Weather.Conditions == 6))
female
difference <- as.integer( male - female)
difference # male 2 less

# Weather conditions: (7) Fog or mist - if hazard
male <- count(subset(wsdata, Sex.of.Casualty == 1 & Weather.Conditions == 7))
male
female <- count(subset(wsdata, Sex.of.Casualty == 2 & Weather.Conditions == 7))
female
difference <- as.integer( male - female)
difference # same

# Weather conditions: (8) Other
male <- count(subset(wsdata, Sex.of.Casualty == 1 & Weather.Conditions == 8))
male
female <- count(subset(wsdata, Sex.of.Casualty == 2 & Weather.Conditions == 8))
female
difference <- as.integer( male - female)
difference # male 2 less

# Weather conditions: (9) Unknown
male <- count(subset(wsdata, Sex.of.Casualty == 1 & Weather.Conditions == 9))
male
female <- count(subset(wsdata, Sex.of.Casualty == 2 & Weather.Conditions == 9))
female
difference <- as.integer( male - female)
difference # male 5 less


# Checking if the  number of casualties increased or decreased over time
clean
date_df <- clean
# Parsing date to be used as year & add only year in column instead of whole date
date <- as.Date(clean$Accident.Date, "%d/%m/%Y")
date_df$year <- strftime(date, "%Y")
date_df
# Year with highest number of casualties & their percentage
tabyl(date_df, year) %>% adorn_pct_formatting(digits = 1)
# Representing as bar chart
BarChart(data = date_df, year, 
         main = "% of casualties each year",
         xlab = "Year",  ylab = "Number of Accidents")


# Draw a plot to explain the relationship between the following:
# Light conditions and severity
# Number of accidents compared with severity(1-3) and lighting condition(1-7) in table
table(clean$Casualty.Severity, clean$Lighting.Conditions)
# Bar chart displaying % of each Casualty.Severity for all (1-7) lighting conditions
BarChart(data = clean, Lighting.Conditions, by = Casualty.Severity,
         main = "% of different Casualty.Severity in all lighting conditions",
         xlab = "Lighting Conditions", ylab = "Number of Accidents",
         legend_labels= c("Fatal", "Serious", "Slight"))

# Weather condition and number of vehicles involved
# Number of vehicles (1-7) involved in accident in different Weather conditions (1-9)
table(clean$Number.of.Vehicles, clean$Weather.Conditions)
# Bar chart displaying % of each Casualty.Severity for all (1-7) lighting conditions
BarChart(data = clean, Weather.Conditions, by = Number.of.Vehicles, 
         main = "No. of vehicles involved in accident with diffirent weather",
         xlab = "Weather.Conditions", ylab = "Number of accidents")

         
### Part 3: Regression ###
# Select data
selected_rd <- clean %>% select (Casualty.Class, Casualty.Severity, Type.of.Vehicle, Weather.Conditions, Age.of.Casualty)
# Function below takes a vector as an argument and returns a binary vector 1 for not missing & 0 for missing value
missingNUM <- function(v)
{
  x <- dim(length(v))
  x[which(!is.na(v))] = 1
  x[which(is.na(v))] = 0
  return(x)
}
# Using the function above to create a variable that will represent 1 as not missing & 0 as missing
selected_rd$missing <- missingNUM (selected_rd$Age.of.Casualty)
selected_rd$missing
# separate training and testing the data for the linear model
trainData<-selected_rd[selected_rd['missing'] == 1,]
trainData
testData<-selected_rd[selected_rd['missing'] == 0,]
testData
# Using the training data to create a multilinear regression to model the relationship 
model <- lm(Age.of.Casualty ~ Casualty.Class + Casualty.Severity + Type.of.Vehicle + Weather.Conditions, data=trainData)
# Predict missing values in the df and replace them
predictions <- model %>% predict (testData)
predictions
selected_rd$Age.of.Casualty[is.na(selected_rd$Age.of.Casualty)]<-as.integer (predictions)
# Exclude missing column before creating model
trainData<- trainData[,-6] 
trainData
testData<- testData[,-6]
testData
# Model summary
summary <- summary(model)
summary
# get model R-squared 
r_2 <- summary$r.squared
r_2
# Model coefficients - standard error, a t-test value and significance
modelCoeffs <- summary$coefficients
modelCoeffs
# Save regression
write.csv(selected_rd,"regression.csv")
view(selected_rd)
# Check for missing value
sum(is.na(selected_rd))






