#-----------------------------------#
# Final Project                      #
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
library(tidyverse)
library(randomForest)
library(caret)
library(haven)
library(maps)
library(janitor)
library(RColorBrewer)
library(pROC)
library(rpart)
library(rpart.plot)
library(ggcorrplot)


#--------------------------------------#
#   Data Importation and Cleaning      #
#-------------------------------------#
# Import the data
acs <- read_dta("acs_2023.dta")

# Convert haven_labelled variables in the dataset
acs_cleaned <- acs %>%
  mutate(across(where(is.labelled), as_factor)) 

# Function to remove numeric prefixes from factor levels
remove_numeric_prefix <- function(factor_var) {
  if (is.factor(factor_var)) {
    levels(factor_var) <- sub("^\\d+\\.\\s*", "", levels(factor_var))
  }
  return(factor_var)
}

# List of categorical variables
categorical_vars <- c("metro", "ownershp", "sex", "marst", "race", "hispan",
                      "citizen", "school", "empstat", "labforce", "classwkr", "educ")

# Apply the function to the cat vars
acs_cleaned <- acs_cleaned %>%
  mutate(across(all_of(categorical_vars), remove_numeric_prefix))

# Recode and filter ownershp
acs_cleaned <- acs_cleaned %>%
  mutate(ownershp_num = case_when(
    ownershp == "n/a" ~ NA_real_,  
    ownershp == "owned or being bought (loan)" ~ 1,
    ownershp == "rented" ~ 0,
    TRUE ~ NA_real_ 
    )) %>%
  filter(!is.na(ownershp_num))

# Clean numeric columns
acs_cleaned <- acs_cleaned %>%
  mutate(across(c(age, uhrswork), ~ as.numeric(sub("^(\\d+)\\.\\s.*", "\\1", .))),
         inctot = na_if(inctot, 9999999),
         hhincome = na_if(hhincome, 9999999))

# Filter the data by age
acs_cleaned <- acs_cleaned %>%
  filter(age >= 18 & age <= 64)


#--------------------------#
# Correlation Analysis     #
#--------------------------#
# Compute correlation matrix for numeric variables
numeric_vars <- acs_cleaned %>% select_if(is.numeric) %>% select(-ownershp_num)
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Visualizing the correlation matrix
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           title = "Correlation Matrix",
           colors = c("darkblue", "white", "blue"), 
           outline.col = "lightgrey",
           show.legend = TRUE) 

# Remove highly correlated features 
high_cor <- findCorrelation(cor_matrix, cutoff = 0.6)
numeric_vars_filtered <- numeric_vars[, -high_cor]

# Replace numeric variables in the dataset with filtered ones
acs_cleaned <- bind_cols(
  acs_cleaned %>% select(-one_of(names(numeric_vars))),
  numeric_vars_filtered
)


#-------------------#
#   Data Cleaning   #
#-------------------#
# State lookup table
state_lookup <- data.frame(
  statefip = c(
    1, 2, 4, 5, 6, 8, 9, 10, 11, 12,
    13, 15, 16, 17, 18, 19, 20, 21, 22, 23,
    24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
    34, 35, 36, 37, 38, 39, 40, 41, 42, 44,
    45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56
  ),
  state_name = c(
    "Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida",
    "Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine",
    "Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
    "New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
    "South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"
  ),
  region = c(
    "South", "West", "West", "South", "West", "West", "Northeast", "South", "South", "South",    # 1-12
    "South", "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South", "Northeast", #13-22
    "South", "Northeast", "Midwest", "Midwest", "South", "Midwest", "West", "Midwest", "West", "Northeast", #23-32
    "Northeast", "West", "Northeast", "South", "Midwest", "Midwest", "South", "West", "Northeast", "Northeast",#33-42
    "South", "Midwest", "South", "South", "West", "Northeast", "South", "West", "South", "Midwest", "West"     #43-53
  ),
  division = c(
    "East South Central",   # Alabama
    "Pacific",              # Alaska
    "Mountain",             # Arizona
    "West South Central",   # Arkansas
    "Pacific",              # California
    "Mountain",             # Colorado
    "New England",          # Connecticut
    "South Atlantic",       # Delaware
    "South Atlantic",       # District of Columbia
    "South Atlantic",       # Florida
    "South Atlantic",       # Georgia
    "Pacific",              # Hawaii
    "Mountain",             # Idaho
    "East North Central",   # Illinois
    "East North Central",   # Indiana
    "West North Central",   # Iowa
    "West North Central",   # Kansas
    "East South Central",   # Kentucky
    "West South Central",   # Louisiana
    "New England",          # Maine
    "South Atlantic",       # Maryland
    "New England",          # Massachusetts
    "East North Central",   # Michigan
    "East North Central",   # Minnesota
    "East South Central",   # Mississippi
    "West North Central",   # Missouri
    "Mountain",             # Montana
    "West North Central",   # Nebraska
    "Mountain",             # Nevada
    "New England",          # New Hampshire
    "Middle Atlantic",      # New Jersey
    "Mountain",             # New Mexico
    "Middle Atlantic",      # New York
    "South Atlantic",       # North Carolina
    "West North Central",   # North Dakota
    "East North Central",   # Ohio
    "West South Central",   # Oklahoma
    "Pacific",              # Oregon
    "Middle Atlantic",      # Pennsylvania
    "New England",          # Rhode Island
    "South Atlantic",       # South Carolina
    "West North Central",   # South Dakota
    "East South Central",   # Tennessee
    "West South Central",   # Texas
    "Mountain",             # Utah
    "New England",          # Vermont
    "South Atlantic",       # Virginia
    "Pacific",              # Washington
    "South Atlantic",       # West Virginia
    "East North Central",   # Wisconsin
    "Mountain"              # Wyoming
  )
)

# Clean statefip column
acs_cleaned <- acs_cleaned %>%
  mutate(statefip = as.numeric(sub("^(\\d+)\\.\\s.*", "\\1", statefip)))

# Joining the state names to the acs_cleaned data
acs_cleaned <- acs_cleaned %>%
  left_join(state_lookup, by = "statefip")

# Filter out rows with missing from education
acs_cleaned <- acs_cleaned %>%
  filter(educ != "3 years of college" & educ != "missing")

# Define income labels
income_labels <- c(
  "Q1 (Lowest)",
  "Q2",
  "Q3",
  "Q4",
  "Q5 (Highest)"
)

# Assigning quintiles to income
acs_cleaned <- acs_cleaned %>%
  mutate(income_group = ntile(hhincome, 5)) %>%            
  mutate(income_group = factor(income_group, levels = 1:5,  
                               labels = income_labels))

# Structure of dataset
str(acs_cleaned)


#---------------------#
#   Data Exploration  #
#---------------------#
# Compute summary for state
home_by_state <- acs_cleaned %>%
  group_by(state_name) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for region
home_by_region <- acs_cleaned %>%
  group_by(region) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for division
home_by_division <- acs_cleaned %>%
  group_by(division, region) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Add 'region' column as lowercase state name
home_by_state$region <- tolower(home_by_state$state_name)

# Create a map of homeownership rate by state
states_map <- map_data("state")

# Get only regions present in the map data
included_states <- unique(states_map$region)
home_by_state <- home_by_state %>% 
  filter(region %in% included_states)

# Merge with your data (as previously shown)
map_data_joined <- left_join(states_map, home_by_state, by = "region")


# Compute summary for age group
acs_cleaned <- acs_cleaned %>%
  mutate(age_group = cut(age, breaks = c(0, 29, 39, 49, 59, 69, Inf),
                         labels = c("<30", "30-39", "40-49", "50-59", "60-69", "70+")))

# Percent homeowners by age group
age_home <- acs_cleaned %>%
  group_by(age_group) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for income quantile
income_home <- acs_cleaned %>%
  group_by(income_group) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Remove NAs and sort by rate
income_home <- income_home %>%
  filter(!is.na(income_group)) %>%
  arrange(desc(homeowner_rate)) %>%
  mutate(income_group = factor(income_group, levels = income_group)) 

# Create homeownership rate by education
educ_home <- acs_cleaned %>%
  group_by(educ) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE)) %>%
  arrange(desc(homeowner_rate))

# Create a custom palette
n_ed_levels <- length(unique(educ_home$educ))
myBlues <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(n_ed_levels)
myBlues <- rev(myBlues)

# Compute summary for race
race_home <- acs_cleaned %>%
  group_by(race) %>% 
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE)) %>%
  arrange(desc(homeowner_rate))

# Compute summary for marital status
marst_home <- acs_cleaned %>%
  group_by(marst) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for metro status
metro_home <- acs_cleaned %>%
  group_by(metro) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for school attendance
school_home <- acs_cleaned %>%
  group_by(school) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for sex
sex_home <- acs_cleaned %>%
  group_by(sex) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for citizenship status
citizen_home <- acs_cleaned %>%
  group_by(citizen) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE)) %>%
  arrange(desc(homeowner_rate)) 

# Compute summary for hispanic status
hispan_home <- acs_cleaned %>%
  group_by(hispan) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for education and marital status
educ_marst_home <- acs_cleaned %>%
  group_by(educ, marst) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for income and race
income_race_home <- acs_cleaned %>%
  group_by(income_group, race) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for metro and classwkr
metro_class_home <- acs_cleaned %>%
  group_by(metro, classwkr) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))

# Compute summary for metro and class of worker
classwkr_race_home <- acs_cleaned %>%
  group_by(classwkr, metro) %>%
  summarize(homeowner_rate = mean(ownershp_num == 1, na.rm = TRUE))


#-------------------------#
#   Data Visualization    #
#-------------------------#
#---------------------#
# Exploratory Plots   #
#---------------------#
# Plot of homeownership rate by state
ggplot(map_data_joined, aes(x = long, y = lat, group = group, fill = homeowner_rate)) +
  geom_polygon(color = "black") +
  scale_fill_continuous(name="Homeownership Rate", labels=scales::percent, 
                        high = "darkblue", 
                        low= "lightblue",
                        guide= guide_colorbar(reverse = TRUE)) +
  labs(title = "Homeownership Rate by State",
       fill = "Homeownership Rate") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# Save the plot
#ggsave("homeownership_map.png", width=10, height=6)

# Plot of homeownership rate by region
ggplot(home_by_region, aes(x = reorder(region, homeowner_rate), y = homeowner_rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = scales::percent(homeowner_rate, accuracy = 0.1)), 
            hjust = -0.1, size = 3.5, color = "black") +
  coord_flip() +
  labs(title = "Homeownership Rate by Region",
       x = "Region",
       y = "Homeownership Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.1)) +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank())
# Save the plot
#ggsave("homeownership_by_region.png", width=6, height=4)

# Plot of homeownership rate by division
ggplot(home_by_division, aes(x = reorder(division, -homeowner_rate), y = homeowner_rate, fill = region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(homeowner_rate, accuracy = 0.1)), 
            vjust = -0.25, size = 3.5, color = "black") +
  labs(title = "Homeownership Rate by Division",
       x = "Division",
       y = "Homeownership Rate",
       fill = "Region") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.1)) +
  scale_fill_brewer(palette = "Blues", direction = -1)  +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank())
# Save the plot
#ggsave("homeownership_by_division.png", width=6, height=4)

# Plot of homeownership rate by income
ggplot(income_home, aes(x = income_group, y = homeowner_rate, fill = income_group)) +
  geom_col(color = "black", width = 0.7) +
  geom_text(aes(label = scales::percent(homeowner_rate, accuracy = 0.1)), 
            vjust = -0.3, size = 3.5, color = "black") +
  labs(
    title = "Homeownership Rate by Income Quintile",
    x = "Income Quintile",
    y = "Homeownership Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(size = 12, face = "bold")
  )
# Save the plot
#ggsave("homeownership_by_income.png", width=6, height=4)

# Plot of homeownership rate by age group
ggplot(age_home, aes(x = age_group, y = homeowner_rate, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Homeownership Rate by Age Group",
       x = "Age Group",
       y = "Homeownership Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank()
  )
#ggsave("homeownership_by_age.png", width=6, height=4)

# Plot homeownership rate by education
ggplot(educ_home, aes(x = reorder(educ, -homeowner_rate), y = homeowner_rate, fill = educ)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label = scales::percent(homeowner_rate, accuracy = 0.1)), vjust = -0.3, size = 3.5, color = "black") +
  labs(
    title = "Homeownership Rate by Education",
    x = "Education Level",
    y = "Homeownership Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = myBlues) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank()
  )
#ggsave("homeownership_by_educ.png", width=6, height=4)

# Plot of homeownership rate by marital status
ggplot(marst_home, aes(x = reorder(marst, -homeowner_rate), y = homeowner_rate, fill = marst)) +
  geom_col(color = "black", width = 0.7) +
  geom_text(aes(label = scales::percent(homeowner_rate, accuracy = 0.1)), vjust = -0.3, size = 3.5, color = "black") +
  labs(
    title = "Homeownership Rate by Marital Status",
    x = "Marital Status",
    y = "Homeownership Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 10, hjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank()
  )
# Save the plot
#ggsave("homeownership_by_marst.png", width=6, height=4)

# Plot of homeownership rate by metro status
ggplot(metro_home, aes(x = reorder(metro, homeowner_rate), y = homeowner_rate)) +
  geom_segment(aes(xend = metro, y = 0, yend = homeowner_rate), color = "grey70", size = 2) +
  geom_point(color = "blue", size = 6) +
  coord_flip() +
  labs(title = "Homeownership Rate by Metropolitan Status",
       x = "Metropolitan Status", y = "Homeownership Rate") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(size = 12, face = "bold")
  )
# Save the plot
#ggsave("homeownership_by_metro.png", width=6, height=4)

# Plot of homeownership rate by school attendance
ggplot(school_home, aes(x = "", y = homeowner_rate, fill = school)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(homeowner_rate, 0.1)), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Homeownership Rate by School Attendance", fill = "School Attendance") +
  scale_fill_brewer(palette = "Blues") +
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )
# Save the plot
#ggsave("homeownership_by_school.png", width=6, height=4)

# Plot of homeownership rate by sex
ggplot(sex_home, aes(x = "", y = homeowner_rate, fill = sex)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(homeowner_rate, 0.1)), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Homeownership Rate by Sex", fill = "Sex") +
  scale_fill_brewer(palette = "Blues") +
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )
# Save plot
#ggsave("homeownership_by_sex.png", width=6, height=4)

# Plot for citizenship status
ggplot(citizen_home, aes(x = homeowner_rate, y = reorder(citizen, homeowner_rate))) +
  geom_segment(aes(x = 0, xend = homeowner_rate, yend = citizen), color = "grey70", size = 3) +
  geom_point(size = 6, color = "blue") +
  labs(title = "Homeownership Rate by Citizenship Status",
       x = "Homeownership Rate",
       y = "Citizenship Status") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )
# Save the plot
#ggsave("homeownership_by_citizen.png", width=6, height=4)

# Plot of homeownership rate by hispanic status
ggplot(hispan_home, aes(x = reorder(hispan, -homeowner_rate), y = homeowner_rate, fill = hispan)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  geom_text(aes(label = scales::percent(homeowner_rate, accuracy = 0.1)), vjust = -0.3) +
  labs(title = "Homeownership Rate by Hispanic Origin", x = "Hispanic Origin", y = "Homeownership Rate") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(size = 12, face = "bold")
  )
# Save the plot
#ggsave("homeownership_by_hispanic.png", width=6, height=4)

# Plot for homeownership by race
ggplot(race_home, aes(x = reorder(race, -homeowner_rate), y = homeowner_rate, fill = race)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label = scales::percent(homeowner_rate, accuracy = 0.1)), vjust = -0.3, size = 3.5, color = "black") +
  labs(
    title = "Homeownership Rate by Race",
    x = "Race",
    y = "Homeownership Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank()
  )
# Save the plot
#ggsave("homeownership_by_race", width=6, height=4)


#---------------------#
# Relationship Plots  #
#---------------------#
# Plot of race + income by homeownership rate
ggplot(income_race_home, aes(x = income_group, y = homeowner_rate, fill = income_group)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~race) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  labs(title = "Homeownership Rate by Income Quintile and Race", x = "Income Quintile", y = "Homeownership Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none",
        axis.title.y = element_text(size = 12, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank())
# Save the plot
#ggsave("income_race_homeownership.png", width=6, height=4)

# Plot of homeownership rate by education and marital status
ggplot(educ_marst_home, aes(x = educ, y = homeowner_rate, fill = educ)) +
  geom_col(position = position_dodge(), width = 0.7) +
  facet_wrap(~marst) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d() +
  labs(title = "Homeownership Rate by Education and Marital Status", x = "Education", y = "Homeownership Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none",
        axis.title.y = element_text(size = 12, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank())
# Save the plot
#ggsave("homeownership_by_educ_marst.png", width=6, height=4)

# Plot for homeownership rate by metro status and class of worker
ggplot(metro_class_home, aes(x = classwkr, y = metro, fill = homeowner_rate)) +
  geom_tile(color = "white", linewidth = 0.7) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = scales::percent) +
  labs(title = "Homeownership Rate: Metro Status x Class of Worker",
       x = "Class of Worker", y = "Metropolitan Status", fill = "Homeowner Rate") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
# Save the plot
#ggsave("homeownership_by_metro_class.png", width=6, height=4)


#---------------------------------#
# Feature Selection and Modeling  #
#---------------------------------#
#----------------------#
# Feature Engineering  #
#----------------------#
# One-hot encoding for categorical variables
dummies <- dummyVars(~ metro + educ + race + marst + sex + citizen + classwkr, data = acs_cleaned)
encoded_features <- data.frame(predict(dummies, newdata = acs_cleaned))

# Combine encoded features with numeric columns
required_columns <- c("ownershp_num", "age", "uhrswork", "hhincome")
missing_columns <- setdiff(required_columns, colnames(acs_cleaned))
if (length(missing_columns) > 0) {
  stop(paste("Missing columns in acs_cleaned:", paste(missing_columns, collapse = ", ")))
}

# Combine encoded features with numeric columns
acs_encoded <- cbind(acs_cleaned[, required_columns], encoded_features)

# Remove columns with all NAs
preprocess <- preProcess(acs_encoded, method = "medianImpute")
acs_encoded <- predict(preprocess, acs_encoded)

# Ensure response variable is a factor
acs_encoded$ownershp_num <- as.factor(acs_encoded$ownershp_num)

#List all features
all_features <- c(required_columns[-1], colnames(encoded_features))

# Filter all features
acs_encoded <- acs_encoded[, c(all_features, "ownershp_num")]

# Verify final dataset
str(acs_encoded)  


#----------------------#
# Data Splitting       #
#----------------------#
# Split data into training and testing sets
set.seed(123)  
trainIndex <- createDataPartition(acs_encoded$ownershp_num, p = 0.7, list = FALSE)
train_data <- acs_encoded[trainIndex, ]
test_data <- acs_encoded[-trainIndex, ]


#----------------------#
# Logistic Regression  #
#----------------------#
# Train Logistic Regression model
logistic_model <- glm(ownershp_num ~ ., data = train_data, family = "binomial")

# Predict on test data
logistic_preds <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to binary class
logistic_class <- ifelse(logistic_preds > 0.5, 1, 0)
logistic_class <- factor(logistic_class, levels = c(0, 1))
summary(logistic_model)

# Evaluation for Logistic Regression
logistic_confusion <- confusionMatrix(logistic_class,test_data$ownershp_num)
# Print confusion matrix
print(logistic_confusion)


# Calculate AUC
logistic_auc <- roc(
  response = test_data$ownershp_num,
  predictor = logistic_preds,
  levels = c(0, 1),
  direction = "<"
)$auc

# Print AUC
print(paste("Logistic Regression AUC:", round(logistic_auc, 4)))

# Plot ROC curve
roc_curve <- roc(
  response = test_data$ownershp_num,
  predictor = logistic_preds,
  levels = c(0, 1),
  direction = "<"
)

plot(roc_curve, 
     main = "ROC Curve for Logistic Regression",
     col = "blue", 
     lwd = 2)
# Add AUC to the plot
legend("bottomright", 
       legend = paste("AUC =", round(logistic_auc, 4)), 
       col = "blue", 
       lwd = 2)
# Save the plot
# ggsave("roc_curve_logistic.png", width=6, height=4)

# Calculate accuracy
log_accuracy <- logistic_confusion$overall["Accuracy"]
# Print accuracy
print(paste("Logistic Regression Accuracy:", round(log_accuracy, 4)))
 
# Print Varible importance
logistic_importance <- varImp(logistic_model)
print(logistic_importance)

# Convert importance object to a data frame
feature_importance <- as.data.frame(logistic_importance$Overall)
feature_importance$Feature <- rownames(feature_importance)
colnames(feature_importance) <- c("Importance", "Feature")

# Sort importance in descending order
feature_importance <- feature_importance[order(-feature_importance$Importance), ]

# Plot top 10 most important features
top_features <- feature_importance[1:10, ]

ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Top 10 Most Important Features",
    x = "Features",
    y = "Importance"
  ) +
  theme_minimal()


#----------------------#
# Random Forest        #
#----------------------#
# Randomly sample 50% from the training data
set.seed(123) 
train_data1 <- train_data %>% 
  group_by(ownershp_num) %>%
  sample_frac(0.5) %>%
  ungroup()

# Ensure response variable is a factor in both train and test datasets
train_data1$ownershp_num <- as.factor(train_data1$ownershp_num)
test_data$ownershp_num <- as.factor(test_data$ownershp_num)

# Train the Random Forest model
rf_model <- randomForest(
  ownershp_num ~ ., 
  data = train_data1, 
  ntree = 100, 
  mtry = 2,
  importance = TRUE  
)

# Predict on the test dataset
rf_preds <- predict(rf_model, newdata = test_data, type = "response")

# Ensure predictions and actual values have the same factor levels
rf_preds <- factor(rf_preds, levels = levels(test_data$ownershp_num))

# Evaluation for Random Forest
rf_confusion <- confusionMatrix(rf_preds, test_data$ownershp_num)
rf_accuracy <- rf_confusion$overall["Accuracy"]

# Compute AUC using pROC
rf_auc <- roc(as.numeric(test_data$ownershp_num), as.numeric(rf_preds))$auc

# Print results
print(paste("Random Forest Accuracy:", round(rf_accuracy, 4)))
print(paste("Random Forest AUC:", round(rf_auc, 4)))

# Optionally, display variable importance
varImpPlot(rf_model)


#----------------------#
# Decision Tree        #
#----------------------#
# Train Decision Tree model
tree_model <- rpart(ownershp_num ~ ., data = train_data, method = "class")

# Predict on test data
tree_preds <- predict(tree_model, newdata = test_data, type = "class")

# Evaluate Decision Tree
tree_confusion <- confusionMatrix(tree_preds, test_data$ownershp_num)
print(tree_confusion)

# Compute AUC for Decision Tree
tree_probs <- predict(tree_model, newdata = test_data, type = "prob")[, 2]
tree_auc <- roc(test_data$ownershp_num, tree_probs, levels = c(0, 1), direction = "<")$auc
print(paste("Decision Tree AUC:", round(tree_auc, 4)))

# Plot the Decision Tree
rpart.plot(tree_model, type = 3, extra = 101, fallen.leaves = TRUE, main = "Decision Tree for Homeownership")

# Accuracy score for Decision Tree
tree_accuracy <- tree_confusion$overall["Accuracy"]

#-------------------------------#
# Model Comparison Table        #
#-------------------------------#
comparison_table <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "Decision Tree"),
  Accuracy = c(logistic_accuracy, rf_accuracy, tree_accuracy),
  AUC = c(logistic_auc, rf_auc, tree_auc)
)

print(comparison_table)