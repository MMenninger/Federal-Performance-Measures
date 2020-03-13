install.packages("tidycensus")
install.packages("sf")
install.packages("rgdal", type = "source")
library(rgdal)
library(tidycensus)
library(tidyverse)
library(purrr)
library(dplyr)
library(ggrepel)

#(you will also need a census api key --> https://walkerke.github.io/tidycensus/reference/census_api_key.html )

## Set Global Variables ----------------------------------------------------
IL_FIPS <- "17"
CMAP_7CO <- c("031", "043", "089", "093", "097", "111", "197") #(Chicago MPO Counties)
years <- lst(2010, 2011,  2013, 2014, 2015, 2016, 2017)
DP03_variables <- c( "DP03_0018", "DP03_0019", "DP03_0020", "DP03_0021", "DP03_0022", "DP03_0023", "DP03_0024" )

## Download the data ----
## Non-SOV Travel (ACS table DP03) --------------------------------------- (run one of these three)
tract <- map_dfr(.x = years, 
                 .f= ~ get_acs(geography="tract", survey="acs5", year= .x , output="wide", variables = DP03_variables,
                               state=IL_FIPS, county=CMAP_7CO, cache_table=TRUE, geometry = TRUE), 
                 .id = "year")


county <- map_dfr(.x = years, 
                  .f= ~ get_acs(geography = "county", variables = DP03_variables, year = .x,  output="wide",
                                state=IL_FIPS, county=CMAP_7CO, survey = "acs5", geometry = TRUE ), 
                  .id = "year") 

# No Geography available for urban areas
#This is the official federal performance measure table and geography
urban_area <- map_dfr(.x = years, 
                      .f= ~ get_acs(geography = "urban area", variables = DP03_variables, year = .x,  output="wide",
                                    survey = "acs5" ), 
                      .id = "year") %>%
  filter(GEOID == "16264")  #Filter for Chicago Region


## Clean up the data ------------
# Calculate the relevant variables for each year
annual_data_cleaned <- urban_area %>%
  #annual_data_cleaned <- county %>%
  #annual_data_cleaned <- tract %>%
  mutate(
    wrkrs_16plus = DP03_0018E,   # Universe: Workers 16 years and over
    drv_alone = DP03_0019E,      # Car, truck, or van: Drove alone
    carpool = DP03_0020E,        # Car, truck, or van: Carpooled
    pub_trans = DP03_0021E,      # Public transportation (excluding taxicab)
    walk = DP03_0022E,           # Walked
    other = DP03_0023E,           # Other means
    work_home = DP03_0024E,      # Worked at home
    nonsov = wrkrs_16plus - drv_alone , 
    nonsov_pct = nonsov/wrkrs_16plus
    #,geometry #(comment out for urban area)
  ) %>%
  select(-starts_with("DP03"))

annual_data_cleaned$year <- as.integer(annual_data_cleaned$year)

county_data_cleaned <- county %>%
  rename(
    wrkrs_16plus = DP03_0018E,   # Universe: Workers 16 years and over
    drv_alone = DP03_0019E,      # Car, truck, or van: Drove alone
    carpool = DP03_0020E,        # Car, truck, or van: Carpooled
    pub_trans = DP03_0021E,      # Public transportation (excluding taxicab)
    walk = DP03_0022E,           # Walked
    other = DP03_0023E,         # Other means
    work_home = DP03_0024E)  %>% # Worked at home 
  mutate(
    nonsov = wrkrs_16plus - drv_alone ,
    nonsov_pct = nonsov/wrkrs_16plus) %>%
  mutate(year = as.integer(year))
  
  

##Plot annual data ----
ggplot(annual_data_cleaned, aes(x= year, y= nonsov_pct))+
  geom_point()+
  geom_text(aes(label= round(nonsov_pct *100, digits = 2)), nudge_y = .002) +
  ggtitle("Share of trips to work via non-SOV modes",
          subtitle="among workers aged 16 and over in the Chicago Urbanized Area") +
  scale_x_continuous("Year",minor_breaks = NULL,  breaks = annual_data_cleaned$year) +
  scale_y_continuous("Share of trips to work", minor_breaks=NULL, labels=scales::percent) +
  theme_minimal() +
  coord_cartesian(ylim=c(0.29, 0.32)) +
  labs(caption="Source: American Community Survey (table DP03)") +
  geom_hline(yintercept=0, color="#888888") +  # Emphasize y=0 for reference (if in plot) +
  geom_line(size=1) 


##Plot county data ----
ggplot(county_data_cleaned, aes(x= year, y= nonsov_pct, group = NAME))+
  geom_point(aes(color=NAME))+
  geom_line(aes(color=NAME)) +
  geom_text(aes(label= round(nonsov_pct *100, digits = 2)), nudge_y = .002) +
  ggtitle("Share of trips to work via non-SOV modes",
          subtitle="among workers aged 16 and over in the Chicago Urbanized Area") +
  scale_x_continuous("Year",minor_breaks = NULL,  breaks = annual_data_cleaned$year) +
  scale_y_continuous("Share of trips to work", minor_breaks=NULL, labels=scales::percent) +
  labs(caption="Source: American Community Survey (table DP03)") 




