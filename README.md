# Stop_and_Frisk_Project

## Housing vs. Stop and Frisk

**Analyze how real estate values correlate stop and frisk frequencies**  

**Data**: NYC Stop and Frisk data , Zillow Housing Value Index (ZHVI)  (time series by neighbourhood or zip code)  

Hypothesis: Higher real estate values related to a lower frequency of stops/lower usage of violence. Moreover, we want to investigate whether the real estate value of neighbouring areas are correlated with the number of stops. In other words, how do two neighbourhoods that have the same real estate value but have a different composition of surrounding neighbourhoods, compare in terms of stops distribution?   

We also want to explore one of the following ideas:  

**Compare the different distribution of house prices/ stops throughout the years that we have data. investigate how stops frequency and distribution varied i neighbourhoods that underwent gentrification or large (relative) change in real estate value during these years.**  

1. Download ZHVI data and clean dataset

  - Standardize variables by year (e.g. z-score)
  - Merge with the Stop and Frisk dataset using long-lat and zip codes
  
2. Literature review and dataset exploration

  - Understand how stop & frisk changed throughout the years
  - Understand what neighbourhoods of nyc have changed the most during specified time-frame

3. For one year, controlling for ZHVI, look at number of stops

  - Subset one year from the dataset. Model spatial distribution of the stops, and the spatial correlation that exist. 
  - This can be accomplished by first just looking at error distribution and spatial correlation (e.g. using Moran I) under a null model with no covariates
  - Then add ZHVI and redo. Moran I test, is there still spatial correlation? 
  
4. Include time variable

  - Either perform the same above for multiple years
  - Include a factor variable in the analysis for each year
  - Could also try using continuous variable for time

5. Visualize change in distribution over time

  - Method used tbd - it would be interesting to show a map with varying covariance structure as times goes by. 
  - Animation of the map with either color change, or a bar for each area (changing height)

