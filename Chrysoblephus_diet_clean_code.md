Chrysoblephus diet data analysis
================
Joshua Dwyer-Thiem
2026-02-24

## Diet data analysis:

### Data wrangling and brief analysis of Chrysoblephus gibbiceps diet data

This document contains the code used in
**Chrysoblephus_diet_raw_script** but was made to present the reasoning
behind the code used and results in a more palatable and descriptive
format. Some code outputs have been hidden for a neater layout, to view
these outputs please refer to **Chrysoblephus_diet_raw_script**.
<br><br><br>

First I loaded the required packages.These were installed via the
**install.R** file.

``` r
library(tidyverse) 
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(ggplot2)
```

The second step was to read in the data (CSV format) then view it to
check for obvious errors and that it was in tidy format.

``` r
#Reading in data as a CSV
data_raw<-read_csv("Data/ChrysoblephusDiet.csv")
```

    ## Rows: 280 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): FishIDnumber, sample_month, sex, Phylum, Lowest classification
    ## dbl (9): sample_year, total_length, fork_length, weighed_fish, Preserved sto...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#looking at raw data 
data_raw 
```

After viewing the raw data, it is clear that it is in tidy format
already; each variable forms a column, each observation forms a row and
each cell is a single measurement. To view the raw data itself and an
explanation of its contents please refer to my **Data** folder.

Although my data were tidy, I spotted a missing value and so decided to
scan for more.

``` r
#scanning for missing values
data_raw %>%
  summarise_all(~sum(is.na(.))) %>% 
  View()
```

The above code revealed multiple missing values across a range of
columns, which could complicate analysis. Therefore the next step was to
remove them:

``` r
#removing missing values
cleaned_data <- data_raw %>% 
  drop_na() %>%
  view()
```

The missing values were removed successfully and the raw data was
transferred to “cleaned_data”. The cleaned data-frame with no missing
values is accessible in my **Data** folder. To check if the above code
was successful I used the following code:

``` r
#Checking if the above worked 
cleaned_data %>%
  summarise_all(~sum(is.na(.))) %>% 
  View()
```

Once the missing values were removed, the data were ready for analysis.
However I was only interested in the relationships between fish body
mass and empty stomach mass and fish body mass and stomach content mass
(all measured in Grams). Therefore I created a new tibble of a subset of
those three variables (stomach_data).

``` r
stomach_data <- cleaned_data %>%
  select(
    `weighed_fish`, `Empty stomach mass (g)`, `Stomach contents (g)`)

#viewing new tibble
stomach_data
```

Once I had my new tibble of body mass, empty stomach mass and stomach
content mass, I planned to create two linear model scatter plots. But
first I wanted to calculate the R² value of the relationship between
fish body mass and empty stomach mass and fish body mass and stomach
content mass, in order to display this on the two plots.

``` r
#calculating R^2 in order to display it on my plots 
# Using cor() squared (Pearson correlation squared)

#R^2 for body mass vs stomach mass
R_squared_plot1 <- cor(stomach_data$weighed_fish,stomach_data$`Empty stomach mass (g)`)^2


#R^2 for body mass vs stomach content mass
R_squared_plot2 <- cor(stomach_data$weighed_fish,stomach_data$`Stomach contents (g)`)^2
```

After I had the R^2 values it was time to construct the two plots using
ggplot. The first plot is body mass vs empty stomach mass. The plot
shows that as body mass increases, so does empty stomach mass.

``` r
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
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Chrysoblephus_diet_clean_code_files/figure-gfm/plot%20of%20body%20mass%20vs%20empty%20stomach%20mass-1.png)<!-- -->

The second plot is Body mass vs stomach content mass. This plot shows
that as body mass increases stomach content mass seems to roughly
increase too. This would make sense as bigger fish would be able to eat
bigger and more prey.

``` r
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
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Chrysoblephus_diet_clean_code_files/figure-gfm/plot%20of%20body%20mass%20vs%20stomach%20content%20mass-1.png)<!-- -->
