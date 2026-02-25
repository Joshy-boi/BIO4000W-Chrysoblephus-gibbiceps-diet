library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)
#Reading in data as a CSV
data_raw<-read_csv(here("Data","ChrysoblephusDiet.csv"))

#looking at raw data 
data_raw 

#data is tidy in terms of structure- each variable forms a column, 
#each observation forms a row, each cell is a single measurement. 


#scanning for missing values
data_raw %>%
  summarise_all(~sum(is.na(.))) %>% 
  print()

#removing the missing values

cleaned_data <- data_raw %>% 
  drop_na() %>%
  print()

#Checking if the above worked 

cleaned_data %>%
  summarise_all(~sum(is.na(.))) %>% 
  print()

#writing out the above cleaned data set to its own file 

write_csv(cleaned_data, here("Data", "cleaned_ChrysoblephusDiet.csv"))


#going to use slecet to create a subset of the 3 variables of interest and

#then ggplot to make two graphs 

stomach_data <- cleaned_data %>%
  select(
    `weighed_fish`, `Empty stomach mass (g)`, `Stomach contents (g)`)

#viewing new tibble
stomach_data


#calculating R^2 in order to display it on my plots 
# Using cor() squared (Pearson correlation squared)

#R^2 for body mass vs stomach mass

R_squared_plot1 <- cor(stomach_data$weighed_fish,stomach_data$`Empty stomach mass (g)`)^2


#R^2 for body mass vs stomach content mass

R_squared_plot2 <- cor(stomach_data$weighed_fish,stomach_data$`Stomach contents (g)`)^2




#plot 1:
#plotting body weight vs empty stomach mass

ggplot(stomach_data, aes(x = `weighed_fish`, y = `Empty stomach mass (g)`)) +
  geom_point(alpha = 0.6, color = "#1f78b4") +
  geom_smooth(method = "lm", se = FALSE, color = "#33a02c") +
  
  annotate(
    "text",
    x = 1000, y = 25,  
    label = paste0("R² = ", round(R_squared_plot1, 3)),
    size = 5,
    color = "black") +
 
  labs(x = "Body mass (g)", y = "Empty stomach mass(g)",
       title = "Relationship of body mass with stomach mass") +
  theme_minimal(base_size = 12)




#plot 2: 
#plotting body mass vs stomach content mass

ggplot(stomach_data, aes(x = `weighed_fish`, y = `Stomach contents (g)`)) +
  geom_point(alpha = 0.6, color = "#1f78b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  
  annotate(
    "text",
    x = 1000, y = 25,  
    label = paste0("R² = ", round(R_squared_plot1, 3)),
    size = 5,
    color = "black") +
  
  
  labs(x = "Body mass (g)", y = "Mass of stomach contents(g)",
       title = "Relationship of body mass with mass of stomach content") +
  theme_minimal(base_size = 12)
