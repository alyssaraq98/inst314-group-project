#This program has my portion of code for conducting a chi-square test for AirBnB data in New York City.

#Open and Configure Life Expectancy Data File
airBData = read.csv(file.choose(), header=TRUE)
View(airBData)
library("dplyr")

# Count for Brooklyn/Type of Stay
BrooklynFilter = select(
  filter(airBData, neighbourhood_group == "Brooklyn"), c(neighbourhood_group, room_type))
x1 = count(airlineData, "BrooklynFilter$room_type")

# Count for Manhattan/Type of Stay
ManhattanFilter = select(
  filter(airBData, neighbourhood_group == "Manhattan"), c(neighbourhood_group, room_type))
x2 = count(airlineData, "ManhattanFilter$room_type")

# Count for Queens/Type of Stay
QueensFilter = select(
  filter(airBData, neighbourhood_group == "Queens"), c(neighbourhood_group, room_type))
x3 = count(airlineData, "QueensFilter$room_type")

# Count for Bronx/Type of Stay
BronxFilter = select(
  filter(airBData, neighbourhood_group == "Bronx"), c(neighbourhood_group, room_type))
x4 = count(airlineData, "BronxFilter$room_type")

# Count for Staten Island/Type of Stay
StatanIslandFilter = select(
  filter(airBData, neighbourhood_group == "Staten Island"), c(neighbourhood_group, room_type))
x5 = count(airlineData, "StatanIslandFilter$room_type")

#Creating Data frame for chi-square test
homeVector = c(9559, 13199, 2096, 379, 176)
privateVector = c(10132, 7982, 3372, 652, 188)
sharedVector = c(413, 480, 198, 60, 9)

dfObs = data.frame(homeVector, privateVector, sharedVector)

#Chi-squre test
v1 = chisq.test(dfObs) 

#Cramer's V Function for effect size
cvFunction = function(chiValue, n, dfObs) sqrt((chiValue)/(n * dfObs))

#Calculation of Cramer's V
cvAnswer = cvFunction(chiValue = 1559.6, n = 48895, df = 8)

#Bargraph for Chi-Square Test
#Load Data set and library
library(ggplot2)
obsData = read.csv(file.choose(), header=TRUE)
View(obsData)

# Actual Plotting
ggplot(data = obsData, aes(x = Room.Type, y = Observed.Value, fill = Neighborhood))+
  geom_bar(stat='identity', width=0.5, position = "dodge")+
  theme_minimal()+
  scale_fill_brewer(palette='Dark2')+
  geom_errorbar(aes(x = Room.Type, ymin = 0, ymax= Expected.Value), 
                width=0.5, alpha=0.5, size=1, position = "dodge")+
  labs(title = paste("Counts of Observed and Expected Room Types Based on Neighborhoods in NYC"))
