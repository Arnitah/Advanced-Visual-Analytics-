#-----------------------------------#
# QUESTION 1                        #
#-----------------------------------#
#-------------------#
#  Setting WD       #
#-------------------#
# Setting working directory
setwd("C:/Users/Enita/OneDrive - University of West Georgia/Desktop/UWG Course Work/Spring Semester 2025/Advanced Visual Analytics/data")

#--------------------#
#   Libraries        #
#--------------------#
library(ggplot2)
library(plotly)
library(lubridate)
library(zoo)
library(scales)
library(tools)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggpubr)


#--------------------#
# Data Cleaning      #
#--------------------#
# Loading the data
covid <- read.csv("US_covid_cases_March_2023.csv")

# Changing the date variable to date format
covid$date <- ymd(covid$date)

# Sorting the data by date
covid <- covid %>%
  arrange(date)

# Creating a new variable for new cases
covid <- covid %>% mutate(new_cases = cases - lag(cases))


# Creating a new variable for 7-day moving average
covid$ma.7 <- rollmean(covid$new_cases, k = 7, fill= NA, align = "right")

# Removing rows with NA in ma.7
covid <- subset(covid, !is.na(ma.7))

# Getting the minimum and maximum date
min_date <- as.Date("2022-12-24")
max_date <- as.Date("2023-03-23")

# Getting cases for the last 90 days
covid_last_90 <- covid %>%
  filter(date >= min_date & date <= max_date)

# Creating breaks for x-axis
date_labels <- as.Date(c("2023-01-01", "2023-02-01", "2023-03-01"))

# Filling the missing values in new_cases with 0
covid_last_90$new_cases[is.na(covid_last_90$new_cases)] <- 0

# Filling the negative value in new_cases with 0
covid_last_90$new_cases[covid_last_90$new_cases < 0] <- 0

# Rounding the values in new_cases
covid_last_90$new_cases <- round(covid_last_90$new_cases, 0)

# Changing names of columns for better plot representation
covid_last_90 <- covid_last_90 %>%
   rename(
     Date = date,
    `New Cases` = new_cases,
    `Daily Average` = ma.7
  )


#-------------------#
#  Plotting         #
#-------------------#
# Plotting new cases and 7-day moving average
covid_plot <- ggplot(data = covid_last_90, aes(x = Date)) +
  # Bar plot for new cases
  geom_bar(aes(y = `New Cases`), fill = "pink", stat = "identity", alpha = 0.8) +
  # Area under the moving average curve
  geom_area(aes(y = `Daily Average`), fill = "pink", alpha = 0.3) +
  # Line for 7-day moving average
  geom_line(aes(y = `Daily Average`), color = "red3", linewidth = 1, na.rm = TRUE)+
  # Formatting x-axis
  scale_x_date(date_labels = "%b", breaks = date_labels, limits = c(min_date, max_date), expand = c(0.01, 0.01)) +
  # Formatting y-axis
  scale_y_continuous(labels = comma, expand = c(0, 0), limits = c(0, max(covid_last_90$`New Cases`, na.rm = TRUE) * 1.2)) +
  # Labels and title
  labs(title = "New reported cases", x = "", y = "") +
  # Annotation for moving average
  annotate(geom="text", x=as.Date("2023-02-19"), y = 45000, label="7-day\naverage", 
           size = 3, color = "black", hjust = 0.5, vjust = 0.5) +
  # New annotation for spike in cases
  annotate(geom="text", x=as.Date("2023-01-18"), y = 150000, label="New cases", 
           size = 3, color = "black", hjust = 0.5, vjust = 0.5) +
  # Refining theme
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(size=10),
    axis.text.y = element_text(size=10)
  )


# Display the plot
covid_plot


#-----------------------------------#
# QUESTION 2                        #
#-----------------------------------#
# -----------------------------------#
#   Data Cleaning                    #
# -----------------------------------#
# State Economic dataset
state_data <- read.csv("US_state_economic_data_1980_2018.csv")

# subset the data to 2018
state_data_2018 <- subset(state_data, year == 2018)

# checking the structure of the data
summary(state_data_2018$Population)

# checking for missing values
sum(is.na(state_data_2018))

# converting population to numeric
as.numeric(state_data_2018$Population)

# select columns from data set
state_data_2018 <- select(state_data_2018, state_name, Population)

# creating custom breaks for population
break_pop <- c(0, 1000000, 5000000, 10000000, 20000000, Inf)

# creating labels for population
label_pop <- c("<1M", "1M-5M", "5M-10M", "10M-20M", ">20M")

# Categorizing the population
state_data_2018$Population_Category <- cut(state_data_2018$Population, breaks = break_pop, labels = label_pop, include.lowest = TRUE)

# Getting state map data
us.states <- map_data("state")
us.states <- rename(us.states, name=region)
us.states$name <- toTitleCase(us.states$name)

# abbreviating the state names
us.states$state_name <- state.abb[match(us.states$name, state.name)]

# merging the state data with the map data
merged.data <- merge(us.states, state_data_2018, by="state_name")%>%
  filter(!state_name %in% c("AK", "HI"))

# Sort by order
merged.data <- merged.data[order(merged.data$order),]

# -----------------------------------#
#   Data Visualization               #
# -----------------------------------#
# Plotting the map
pop_plot <- ggplot(data = merged.data, aes(x = long, y = lat, group = group, fill = Population_Category)) +
  geom_polygon(color = "black", linewidth = 0.1) +
  scale_fill_manual(values = c("#00BFFF", "#1E90FF", "#4169E1", "#0000CD", "#000080"), name = "Population") +
  labs(title = "Population of US States in 2018") +
  theme_void()

# Display the plot
pop_plot



#-----------------------------------#
# Another dataset                   #
#-----------------------------------#
# Obesity dataset
obesity <- read.csv("obesity_prevalence_2022.csv")

# Checking Prevalence is numeric
obesity$Prevalence <- as.numeric(obesity$Prevalence)

# Exclude the territories
obesity <- subset(obesity, !State %in% c("Alaska", "Hawaii"))

# Merge the obesity data with the map data
merged.data.obesity <- merge(us.states, obesity, by.x = "name", by.y = "State")

# Sort by order 
merged.data.obesity <- merged.data.obesity[order(merged.data.obesity$order),]

# Defining breaks for obesity
break_obesity <- c(0, 20, 25, 30, 35, 40, 45, 50, Inf)

# Defining labels for obesity
label_obesity <- c("<20%", "20%-<25%", "25%-<30%", "30%-<35%", "35%-<40%", "40%-<45%", "45%-<50%", "50%+")

# Categorizing the obesity
merged.data.obesity$Obesity_Category <- cut(merged.data.obesity$Prevalence, breaks = break_obesity, labels = label_obesity, right = FALSE, include.lowest = TRUE)


# Plotting the map
obesity_plot <- ggplot(data = merged.data.obesity, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Obesity_Category), color = "black", linewidth = 0.2) +
  coord_fixed(1.3)+
  scale_fill_manual(values = c("#90EE90", "#FFFF00", "#FFA500", "#FF0000", "#B22222","#A52A2A", "#8B0000",  "#000000"), name = "", na.value = "grey") +
  labs(title = "Map: Overall Obesity", subtitle = "Prevalence of Obesity Based on Self-Reported Weight and Height Among U.S. Adults by State and Territory, BRFSS, 2022") +
  theme_void()+
  theme(legend.position = "right")

# Display the plot
obesity_plot



#-----------------------------------#
# QUESTION 3                        #
#-----------------------------------#
# Load the dataset
ipeds <- read.csv("ipeds_2017.csv")

# Filter columns for California 
ipeds_ca <- subset(ipeds, STABBR == "CA")%>%
  select(INSTNM, ADDR, CITY,STABBR, ZIP, WEBADDR,SECTOR, LONGITUD, LATITUDE, GROFFER)

# Filter for SECTOR 1 and 2
ipeds_ca1 <- ipeds_ca %>% subset(SECTOR == 1 | SECTOR == 2)

# Filter for both private and public institutions
ipeds_capub <- ipeds_ca1 %>% subset(SECTOR == 1)
ipeds_capri <- ipeds_ca1 %>% subset(SECTOR == 2)


# Create a leaflet map
contentpub <- paste(ipeds_capub$INSTNM, "<br/>",
                    ipeds_capub$ADDR, "<br/>",
                    ipeds_capub$CITY, ",", ipeds_capub$STABBR, ipeds_capub$ZIP, "<br/>",
                    ipeds_capub$WEBADDR
)


contentpri <- paste(ipeds_capri$INSTNM, "<br/>",
                    ipeds_capri$ADDR, "<br/>",
                    ipeds_capri$CITY, ",", ipeds_capri$STABBR, ipeds_capri$ZIP, "<br/>",
                    ipeds_capri$WEBADDR
)


ca_plot <- leaflet()%>%addTiles()%>%
  addMarkers(data=ipeds_capub, lat=~LATITUDE, lng=~LONGITUD, popup = contentpub, group="Public 4-year colleges")%>%
  addMarkers(data=ipeds_capri, lat=~LATITUDE, lng=~LONGITUD, popup = contentpri, group="Private 4-year colleges")%>%
  addLayersControl(baseGroups =c("Public 4-year colleges", "Private 4-year colleges"),
                   options=layersControlOptions(collapsed = FALSE))

print(ca_plot)


#-----------------------------------#
# QUESTION 3B                       #
#-----------------------------------#
# Define colors based on GROFFER
ipeds_capub$color <- ifelse(ipeds_capub$GROFFER == 1, "green", "blue")
ipeds_capri$color <- ifelse(ipeds_capri$GROFFER == 1, "green", "blue")


# Create a leaflet map
cag_plot <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = ipeds_capub, lat = ~LATITUDE, lng = ~LONGITUD, 
                   popup = contentpub, group = "Public 4-year colleges",
                   color = ~color, fillOpacity = 0.7, radius = 5) %>%
  addCircleMarkers(data = ipeds_capri, lat = ~LATITUDE, lng = ~LONGITUD, 
                   popup = contentpri, group = "Private 4-year colleges",
                   color = ~color, fillOpacity = 0.7, radius = 5) %>%
  addLayersControl(
    overlayGroups = c("Public 4-year colleges", "Private 4-year colleges"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Display the map
print(cag_plot)
