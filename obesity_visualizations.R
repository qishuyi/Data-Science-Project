install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("maps")

library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)


# Users Must Read!!
# TODO before you start:
# Import the Women, Infants and Children dataset and name it "obesity"
# Import the policies and environment dataset and name it "policies"

# StartYear, EndYear, LocationDesc, Question, Data_Value
red_obesity <- select(obesity, 1, 2, 4, 8, 11)
red_policies <- select(policies, 1,2, 4, 8, 11)

# We have noticed that all entries in obesity have the same YearStart and YearEnd
# and that only 52 entries in policies are from 2002 to 2007
# We decided to ignore the 52 entries because they are all for a specific research question -- "State-level farm to school/preschool policy"
# The resulting dataset will only contain data for surveys that extend within the same year
red_obesity <- filter(red_obesity, as.numeric(YearStart) == as.numeric(YearEnd))
red_policies <- filter(red_policies, as.numeric(YearStart) == as.numeric(YearEnd))

# Only use EndYear (because StartYear = EndYear)
red_obesity <- select(red_obesity, 2:5)
red_policies <- select(red_policies, 2:5)

# Rename "EndYear" to "Year"
colnames(red_obesity)[1] <- "Year"
colnames(red_policies)[1] <- "Year"

# Make each research/survey question its own column
red_policies <- spread(data=red_policies, key = Question, value = Data_Value)

#  We want to look at the correlation between obesity, overweight classification and weight-for-length
## Only use data for obesity
obesity_data <- filter(red_obesity, Question == "Percent of WIC children aged 2 to 4 years who have obesity")
overweight_data <- filter(red_obesity, Question == "Percent of WIC children aged 2 to 4 years who have an overweight classification")
infant_data <- filter(red_obesity, Question == "Percent of WIC children aged 3-23 months old who have a high weight-for-length")

## Take the mean of the rows with the same state and year
obesity_by_state_year <- group_by(obesity_data, Year, LocationDesc)
red_obesity <- summarize(obesity_by_state_year, Obesity_Rate = mean(Data_Value, na.rm=TRUE))
overweight_by_state_year <- group_by(overweight_data, Year, LocationDesc)
red_overweight <- summarize(overweight_by_state_year, Overweight_Classifcation = mean(Data_Value, na.rm=TRUE))
infant_by_state_year <- group_by(infant_data, Year, LocationDesc)
red_infant <- summarize(infant_by_state_year, Percent_of_high_weight_for_length = mean(Data_Value, na.rm=TRUE))

## We will only look at the correlation between the three variables in the year of 2012
red_obesity <- filter(red_obesity, Year == 2012)
red_overweight <- filter(red_overweight, Year == 2012)
red_infant <- filter(red_infant, Year == 2012)

## Merge the three datasets together
data_for_correlation <- inner_join(red_obesity, red_overweight, by = c('Year', 'LocationDesc'))
data_for_correlation <- inner_join(data_for_correlation, red_infant, by = c('Year', 'LocationDesc'))

## Take the correlation between obesity and overweight classification to justify that we choose one response variable among the three
## There is a strong positive correlation between overweight and obesity (r = 0.6192738)
## There is also a strong positive correlation between weight-for-length and obesity (r = 0.758281)
cor(data_for_correlation$Obesity_Rate, data_for_correlation$Overweight_Classifcation)
cor(data_for_correlation$Obesity_Rate, data_for_correlation$Percent_of_high_weight_for_length)

# Merge two dataframes together
obesity2012 <- inner_join(x = red_obesity, y = red_policies, by = c("Year", "LocationDesc"))

# Select only the useful columns of the dataset
obesity2012 <- select(obesity2012, 1:3, 6, 11, 25)

# Rename columns to shorter names so that we can refer to them easier
colnames(obesity2012)[4] <- "Farmers_Market"
colnames(obesity2012)[5] <- "SNAP_Benefits"
colnames(obesity2012)[6] <- "Complete_Street"

# Make sure the values are numeric
obesity2012$Farmers_Market <- as.numeric(as.character(obesity2012$Farmers_Market))
obesity2012$Obesity_Rate <- as.numeric(as.character(obesity2012$Obesity_Rate))
obesity2012$SNAP_Benefits <- as.numeric(as.character(obesity2012$SNAP_Benefits))

# Create a new column for the number of farmers markets per 100,000 residents with SNAP benefits
obesity2012 <- mutate(obesity2012, Farmers_Market_With_SNAP = Farmers_Market * SNAP_Benefits / 100)
obesity2012 <- na.omit(obesity2012)

# Create a map of the 50 main states
MainStates <- map_data("state")

# Operate on a copy of obesity2012
MapDat <- obesity2012

# Lowercase the dataframe under the LocationDesc column
MapDat$LocationDesc <- tolower(MapDat$LocationDesc)

# Make a map based on a column in Mainstates and a column in obesity2012
MapDat <- inner_join(MainStates, MapDat, by = c("region" = "LocationDesc"))

# Only use the columns that we need and make sure the values are numeric
MapDat <- select(MapDat,1:5, 8:12)
MapDat$Farmers_Market <- as.numeric(as.character(MapDat$Farmers_Market))

# Create the maps and adjust the labels
Farmers_Market_Map <- ggplot() + geom_polygon( data=MapDat, 
                                               aes(x=long, y=lat, group=group, fill = Farmers_Market), 
                                               color="white", size = 0.2)

Farmers_Market_Map <- Farmers_Market_Map + scale_fill_continuous(name="Number per 100,000 residents", 
                                                                 low = "lightblue", high = "darkblue", na.value = "grey50") + labs(title="Farmers' Markets in the Mainland United States, 2012")

Obesity_Map <- ggplot() + geom_polygon(data=MapDat, 
                                       aes(x=long, y=lat, group=group, fill = Obesity_Rate), 
                                       color="white", size = 0.2)

Obesity_Map <- Obesity_Map + scale_fill_continuous(name="Obesity Rate(Percent)", 
                                                   low = "lightblue", high = "darkblue", na.value = "grey50") + labs(title="WIC Obesity Rates in the Mainland United States, 2012")

Farmers_Market_SNAP_Map <- ggplot() + geom_polygon( data=MapDat, 
                                                    aes(x=long, y=lat, group=group, fill = Farmers_Market_With_SNAP), 
                                                    color="white", size = 0.2)

Farmers_Market_SNAP_Map <- Farmers_Market_SNAP_Map + scale_fill_continuous(name="Number per 100,000 residents", 
                                                                 low = "lightblue", high = "darkblue", na.value = "grey50") + labs(title="Farmers' Markets with SNAP Benefits in the Mainland United States, 2012")
# Show the maps
Farmers_Market_Map
Obesity_Map
Farmers_Market_SNAP_Map

# Make a scatterplot with number of farmers' markets with SNAP, obesity rate and colored by whether a state has complete streets policy
scat <- ggplot(data=obesity2012) +
        geom_point(mapping = aes(x=Farmers_Market_With_SNAP, y=Obesity_Rate, color=Complete_Street)) +
        labs(x = "Number of farmers' market with SNAP (per 100,000 residents)", 
             y = "Obesity Rate(Percent)",
             color = "Adopted completed streets policy",
             title="WIC Obesity Rate in the United States, 2012")

scat