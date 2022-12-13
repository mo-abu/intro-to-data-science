library(dplyr)
library(lubridate)
library(lessR)
library(tidyverse)
library(janitor)

#read csv
accidents <- read.csv("data/accidents.csv")
accidentsCopy <- accidents

#info on csv
str(accidentsCopy)
summary(accidentsCopy)
sapply(accidentsCopy, typeof)
dim(accidentsCopy)
nrow(accidentsCopy)
ncol(accidentsCopy)
names(accidentsCopy) 

# checking missing value in columns and select all rows where a variable is none and record as NA
any(is.na(accidentsCopy))
sum(is.na(accidentsCopy)) 
colSums(is.na(accidentsCopy))
missingData <- accidentsCopy[!complete.cases(accidentsCopy),]

#finding the unique values in road surface / assigning number to match road surface
unique(accidentsCopy$Road.Surface)
accidentsCopy <- accidentsCopy %>% 
  mutate(Road.Surface = sub("Dry", "1", Road.Surface)) %>% 
  mutate(Road.Surface = sub("Wet/Damp", "2", Road.Surface)) %>%
  mutate(Road.Surface = sub('Wet <a8> Damp', "2", Road.Surface)) %>%  
  mutate(Road.Surface = sub("Wet", "2", Road.Surface)) %>% 
  mutate(Road.Surface = sub("Snow", "3", Road.Surface)) %>% 
  mutate(Road.Surface = sub("Frost/Ice", "4", Road.Surface)) %>% 
  mutate(Road.Surface = sub("Ice", "4", Road.Surface))
unique(accidentsCopy$Road.Surface)

#Finding the unique values in 1st.Road.Class 
#to its matching number from "Guidance" USING EXPRESSION LANGUAGE 
unique(accidentsCopy$X1st.Road.Class)
accidentsCopy
accidentsCopy <- accidentsCopy %>% 
  mutate(X1st.Road.Class = sub("M\\d{1,6}", "1", X1st.Road.Class)) %>% 
  mutate(X1st.Road.Class = sub("A\\d{1,6}\\(M)", "2", X1st.Road.Class)) %>% 
  mutate(X1st.Road.Class = sub("A\\d{1,6}", "3", X1st.Road.Class)) %>% 
  mutate(X1st.Road.Class = sub("B\\d{1,6}", "4", X1st.Road.Class)) %>% 
  mutate(X1st.Road.Class = sub("U", "6", X1st.Road.Class))

#Checking if the anomalies are fixed
unique(accidentsCopy$X1st.Road.Class)
accidentsCopy

#removing unnecessarily columns
str(accidentsCopy)
accidentsNew <- accidentsCopy%>%select(-c("Daylight.Dark","Local.Authority"))
accidentsNew


#three sigma Rule
#standard deviation for age of casualty & Mean for age of casualty
outliersSigma <- accidentsNew
outliersSigma = na.omit(accidentsNew)
sdAge <- sd(outliersSigma$Age.of.Casualty, na.rm = "TRUE")
meanAge <- mean(outliersSigma$Age.of.Casualty, na.rm = "TRUE")
sdAge
meanAge

# calculate upper and lower bounds
upper.bound <- meanAge + (3 * sdAge)
lower.bound <- meanAge - (3 * sdAge)
upper.bound
lower.bound

# Extract outliers
outliersSig <- outliersSigma  %>% 
  filter((outliersSigma$Age.of.Casualty > upper.bound) | (outliersSigma$Age.of.Casualty < lower.bound))
outliersSig$Age.of.Casualty

# Hampel Identifier
# Calculate median and MAD
medianValue <- median(accidentsNew$Age.of.Casualty, na.rm = "TRUE")
MADvalue <- mad(accidentsNew$Age.of.Casualty, na.rm = "TRUE")
medianValue
MADvalue
# Calculate upper and lower bounds
upper.bound <- medianValue + 3*MADvalue
lower.bound <- medianValue - 3*MADvalue
upper.bound
lower.bound
# Extract outliers found be Hampel identifier
outliersHampel <- accidentsNew %>%
  filter((Age.of.Casualty > upper.bound)| (Age.of.Casualty < lower.bound))
outliersHampel$Age.of.Casualty

# Boxplot 
boxplot(accidentsNew$Age.of.Casualty)
outlier <- boxplot(accidentsNew$Age.of.Casualty, plot=TRUE)$out
outlier

# Removing outliers from the dataset
# Rows with outliers  
accidentsNew[which(accidentsNew$Age.of.Casualty %in% outlier),]

# Rows containing the outliers
cleanAccident <- accidentsNew[-which(accidentsNew$Age.of.Casualty %in% outlier),]
# Checking boxplot to see if outliers are gone
boxplot(cleanAccident$Age.of.Casualty)
View(cleanAccident)

# save
write.csv(cleanAccident, "data/clean_accident.csv")



































### Part 2. Exploration ####
cleanAccident
names(cleanAccident)

# Comparing male & female casualties with weather Conditions
# Filter Weather.Conditions and Sex.of.Casualty

wsdata <- cleanAccident[,c("Weather.Conditions", "Sex.of.Casualty")]
# Number of male & female casualties in each weather conditions and their %
# Table showing Number of Accidents caused by male(1) & female(2) in each weather conditions
table(wsdata$Sex.of.Casualty, wsdata$Weather.Conditions)
# % of male & female casualties in each weather conditions 
BarChart(data = cleanAccident, Weather.Conditions, by = Sex.of.Casualty,
         main = "% of male & female casulties in each weather conditions",
         legend.labels = c("male", "female"), 
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
cleanAccident
date.df <- cleanAccident
# Parsing date to be used as year & add only year in column instead of whole date
date <- as.Date(cleanAccident$Accident.Date, "%d/%m/%Y")
date.df$year <- strftime(date, "%Y")
date.df
# Year with highest number of casualties & their percentage
tabyl(date.df, year) %>% adorn_pct_formatting(digits = 1)
# Representing as bar chart
BarChart(data = date.df, year, 
         main = "% of casualties each year",
         xlab = "Year",  ylab = "Number of Accidents")


# Draw a plot to explain the relationship between the following:
# Light conditions and severity
# Number of accidents compared with severity(1-3) and lighting condition(1-7) in table
table(cleanAccident$Casualty.Severity, cleanAccident$Lighting.Conditions)
# Bar chart displaying % of each Casualty.Severity for all (1-7) lighting conditions
BarChart(data = cleanAccident, Lighting.Conditions, by = Casualty.Severity,
         main = "% of different Casualty.Severity in all lighting conditions",
         xlab = "Lighting Conditions", ylab = "Number of Accidents",
         legend.labels= c("Fatal", "Serious", "Slight"))

# Weather condition and number of vehicles involved
# Number of vehicles (1-7) involved in accident in different Weather conditions (1-9)
table(cleanAccident$Number.of.Vehicles, cleanAccident$Weather.Conditions)
# Bar chart displaying % of each Casualty.Severity for all (1-7) lighting conditions
BarChart(data = cleanAccident, Weather.Conditions, by = Number.of.Vehicles, 
         main = "No. of vehicles involved in accident with diffirent weather",
         xlab = "Weather.Conditions", ylab = "Number of accidents")
















### Part 3: Regression ###

# Select data
selected.rd <- cleanAccident %>% select (Casualty.Class, Casualty.Severity, Type.of.Vehicle, Weather.Conditions, Age.of.Casualty)
# Function below takes a vector as an argument and returns a binary vector 1 for not missing & 0 for missing value
missingNUM <- function(v)
{
  x <- dim(length(v))
  x[which(!is.na(v))] = 1
  x[which(is.na(v))] = 0
  return(x)
}
# Using the function above to create a variable that will represent 1 as not missing & 0 as missing
selected.rd$missing <- missingNUM (selected.rd$Age.of.Casualty)
selected.rd$missing
# separate training and testing the data for the linear model
trainData<-selected.rd[selected.rd['missing'] == 1,]
trainData
testData<-selected.rd[selected.rd['missing'] == 0,]
testData
# Using the training data to create a multilinear regression to model the relationship 
model <- lm(Age.of.Casualty ~ Casualty.Class + Casualty.Severity + Type.of.Vehicle + Weather.Conditions, data=trainData)
# Predict missing values in the df and replace them
predictions <- model %>% predict (testData)
predictions
selected.rd$Age.of.Casualty[is.na(selected.rd$Age.of.Casualty)]<-as.integer (predictions)
# Exclude missing column before creating model
trainData<- trainData[,-6] 
trainData
testData<- testData[,-6]
testData
# Model summary
summary <- summary(model)
summary
# get model R-squared 
r.2 <- summary$r.squared
r.2
# Model coefficients - standard error, a t-test value and significance
modelCoeffs <- summary$coefficients
modelCoeffs
# Save regression
write.csv(selected.rd,"data/regression.csv")
view(selected.rd)
# Check for missing value
sum(is.na(selected.rd))





