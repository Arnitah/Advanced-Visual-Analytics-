#---------------------------------------------
# Enita Omuvwie Homework 1
#---------------------------------------------
# This script contains the solutions to the
# homework problems for week 1 of the 
# Advanced Visual Analytics course.

#---------------------------------------------
# Libraries 
#---------------------------------------------
library(tidyverse)
library(ggplot2)
library(readr)

#---------------------------------------------
# Question 1
#---------------------------------------------
# Problem 1.1
# Creating a vector with two elements
x <- c(3,4)

# Checking for which number is larger using an if statement
if (x[1] > x[2]) {
  print("The first number is larger!")
} else {
  print("The second number is larger!")
}

# Problem 1.2
# Creating a vector with three elements
x <- c(7,11,3)

# Checking for the median of the vector using if statement with new object median_3
if ((x[1] > x[2] & x[1] < x[3]) | (x[1] < x[2] & x[1] > x[3])) {
  median_3 <- x[1]
} else if ((x[2] > x[1] & x[2] < x[3]) | (x[2] < x[1] & x[2] > x[3])) {
  median_3 <- x[2]
} else {
  median_3 <- x[3]
}

# printing out the median
print(median_3)


# Problem 1.3
# Create a grade place holder 
scores <-  93 

# Create an if loop to check the grade of each student
if (scores >= 90){
  grade <- "A"
} else if (scores >= 80){
  grade <- "B"
} else if (scores >= 70){
  grade <- "C"
} else if (scores >= 60){
  grade <- "D"
} else {
  grade <- "F"
}

print(paste("The letter grade for the score is:",grade))



#---------------------------------------------
# Question 2
#---------------------------------------------
# Problem 2.1
# Creating  a vector of 100 random numbers
x <- rnorm (100, mean = 0, sd = 1)


#Creating a counter for values > 0
cnt <- 0

# For loop to loop through the elements in the vector
for (i in 1:length(x)){
  
  if(x[i] > 0){
    
    cnt <- cnt + 1
    
  }
  
}

# Printing out positive numbers in vector
print(paste("The number of positive numbers in the vector is:",cnt))



# Problem 2.2
x <- sample(1:100, 100, replace = TRUE)

# Initializing the first element as the largest
large <- x[1]


# For loop to loop through the elements in the vector
for (i in 2:length(x)){
  
  # Using if statement to check for the largest element
  
  if(x[i] > large){
    
    large <- x[i]
    
  }
  
}

# Printing out the largest element in the vector
print(paste("The largest element in the vector is:",large))


# Problem 2.3
# Creating a matrix with 5 rows and 5 columns
x <- matrix(1:25, nrow = 5, ncol = 5)

# Sum of the third column
sum_col3 <- sum(x[,3])


# Print the result
print(paste("The sum of the third column is:",sum_col3))


# Problem 2.4
# Creating a matrix with 5 rows and 5 columns
x <- matrix(1:25, nrow = 5, ncol = 5)

# Sum all the elements in the matrix using loops
sum_all <- 0

for (i in 1:5){
  for (j in 1:5){
    sum_all <- sum_all + x[i,j]
  }
}

# Print the result
print(paste("The sum of all elements in the matrix is:",sum_all))


#---------------------------------------------
# Question 3
#---------------------------------------------
# Problem 3.1
# Set working directory
setwd("C:/Users/Enita/OneDrive - University of West Georgia/Desktop/UWG Course Work/Spring Semester 2025/Advanced Visual Analytics/data")

# Load the data
GDP_data <- read.csv("GDP_per_capita.csv")

# Subsetting the data to only include countries in north america
north.america <- subset(GDP_data, Country.Name %in% c("United States","Canada", "Mexico"))

# Cleaning column names for the years
colnames(north.america) <- gsub("X","",colnames(north.america))

# Problem 3.2
# Creating a new dataframe with year between 2000-2010 and another with years after 2010
north.america.1 <- north.america[,c(1,which(colnames(north.america) %in% as.character(2000:2010)))]
north.america.2 <- north.america[,c(1,which(colnames(north.america) %in% as.character(2011:2022)))]

# Print both dataframes
print(north.america.1)
print(north.america.2)

# Problem 3.3
# Merging both dataframes together
north.america.3 <- merge(north.america.1, north.america.2, by = "Country.Name")

#Print the merged dataframe
print(north.america.3)


#---------------------------------------------
# Question 4
#---------------------------------------------
# Problem 4.1
# Load the data
national_welfare <- read.csv("ukcpr_national_welfare_data.csv")

# Print dataframe
print(national_welfare)

# Creating a df with certain variables
national_welfare_1 <- national_welfare[, c("state_name", "state_fips", "year", "Population", "Employment",
                                           "Unemploymentrate", "GrossStateProduct","FederalMinimumWage", "StateMinimumWage")]

# Print the new dataframe
print(national_welfare_1)


# Problem 4.2
# Subsetting df to year 200-2019
national_welfare_data_2000_2019 <- subset(national_welfare_1, year >= 2000 & year <= 2019 
                                          & !state_name %in% c("AK", "HI","WA"))

# Print the subsetted dataframe
print(head(national_welfare_data_2000_2019))

# Saving it into a new csv file
write.csv(national_welfare_data_2000_2019, file = "national_welfare_2000_2019.csv")


# Problem 4.3
# Creating an indicator to compare state and federal mininum wages in 2017
national_welfare_data_2017 <- subset(national_welfare_data_2000_2019, year == 2017) 

national_welfare_data_2017$Higher_Wage <- ifelse(national_welfare_data_2017$StateMinimumWage > national_welfare_data_2017$FederalMinimumWage, 1, 0)

# Count number of states with higher minimum wage
Count_Higher_Wage <- sum(national_welfare_data_2017$Higher_Wage)

# Print the count
print(paste("The number of states with higher minimum wage than the federal minimum wage in 2017 is:",Count_Higher_Wage))


# Problem 4.4
# Creating employment-to-population ratio and GDP per capita for each state from 2000-2019
national_welfare_data_ratio <- national_welfare_data_2000_2019 %>%
  mutate(Employment_to_Population_Ratio = Employment/Population,
         GDP_Per_Capita = GrossStateProduct/Population)

# Finding the state with the highest avg GDP between 2000-2019
Highest_Avg_GDP <- national_welfare_data_ratio %>%
  group_by(state_name) %>%
  summarise(Avg_GDP = mean(GDP_Per_Capita)) %>%
  filter(Avg_GDP == max(Avg_GDP))

# Print the state with the highest avg GDP
print(paste("The state with the highest average GDP per capita between 2000-2019 is:",Highest_Avg_GDP$state_name))


#---------------------------------------------
# Question 5
#---------------------------------------------
# Problem 5.1
# Subsetting the data to be able to plot the histogram distribution
national_welfare_data_2019 <- subset(national_welfare_data_2000_2019, year == 2019)

# Plotting a histogram based on dataset
h1 <- ggplot(national_welfare_data_2019, aes(x=Unemploymentrate)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Unemployment Rate Distribution in 2019",
       x = "Unemployment Rate",
       y = "Frequency")+
  theme_classic()
h1

# Problem 5.2
# Create a subset of the data between year 2010-2019
national_welfare_data_2010_2019 <- subset(national_welfare_data_2000_2019, year >= 2010 & year <= 2019)

# Calculating the Average population for each state
Avg_Population <- national_welfare_data_2010_2019 %>%
  group_by(state_name) %>%
  summarise(Avg_Population = mean(Population))

# Plotting the average population for each state
p1 <- ggplot(Avg_Population, aes(x = state_name, y = Avg_Population)) +
  geom_col(fill = "blue") +
  labs(title = "Average Population for each State between 2010-2019",
       x = "State",
       y = "Average Population") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.0,vjust = 6.5))+
  theme_classic()
p1


# Problem 5.3
# Creeating a line graph to show unemployment rates in Georgia between 2000-2019
Georgia_Unemployment <- national_welfare_data_2000_2019 %>%
  filter(state_name == "GA")

# Plotting the line graph
p3 <- ggplot(Georgia_Unemployment, aes(x = year, y = Unemploymentrate)) +
  geom_line(color = "blue") +
  labs(title = "Unemployment Rate in Georgia between 2000-2019",
       x = "Year",
       y = "Unemployment Rate") +
  theme_classic()
p3

# Problem 5.4
# Filtering the data to only include certain states
national_welfare_data_south <- national_welfare_data_2000_2019 %>%
  filter(state_name %in% c("AL", "GA","FL", "NC", "SC")& year >= 2000 & year <= 2019)

# Plotting a line graph to show unemployment rate for each state between 2000-2019
p2 <- ggplot(national_welfare_data_south, aes(x = year, y = Unemploymentrate, color = state_name)) +
  geom_line() +
  labs(title = "Unemployment Rate for Southern States between 2000-2019",
       x = "Year",
       y = "Unemployment Rate") +
  theme_classic()
p2
