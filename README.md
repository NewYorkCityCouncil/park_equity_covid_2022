## Park Equity & COVID-19
This project was intentionally meant to present data analysis and visuals for NYCC 4.22.22 ['Oversight: The Effect of COVID-19 on Park Equity'](https://legistar.council.nyc.gov/MeetingDetail.aspx?ID=951908&GUID=4D8FDC0B-C36E-4C0D-9346-392A41B04110&Options=info|&Search=) hearing. This new branch builds upon the original project with updated data and methodology, leaving out COVID-19 metrics and instead bringing in more equity-related ones.

An associated webpage for this analysis can be found [on the council website](https://council.nyc.gov/data/data-team/park-equity-covid-2022/), but please note that it has not been updated with the new analysis in this branch.

***  

### Data Sources 
- [Walk-to-a-Park Service Area](https://data.cityofnewyork.us/Recreation/Walk-to-a-Park-Service-area/5vb5-y6cv)
- [Updated 2024 Parks Maintenance Report](https://www.nycgovparks.org/pagefiles/204/Admin-Code-18-144-FY24Report-vf__67d83e05078a0.pdf)
- [Updated 2020 Census Tracts](https://data.cityofnewyork.us/City-Government/2020-Census-Tracts/63ge-mke6/about_data)
- 2022 5-Year ACS Survey: We used R package censusapi to get demographic data needed at the Census Tract level, and the councilcount package for data at the Council District level.

### Methodology 

#### Summary & Intention
- Calculate how much functional acreage of park space residents of NYC have access to.
- Analyze geographic, income, and demographic disparities (including race, youth populations, senior populations, and public assistance households) in access to park space at the 2023 City Council District level.


#### Parks included in Analysis
From the [Annual Report on Park Maintenance (Local Law 98 of 2015)](https://www.nycgovparks.org/news/archive)

#### 10 Minute Walking Distance
Using the access points from Walk-to-a-Park Service Area dataset, we created isochrone polygons or time-distance areas for each point. We used mapbox api for this process and selected a 10 minute walking distance parameter. If the center of a census tract is within 10-minutes walking of any access point associated with a given park, then it is designated as having access to that park. 

#### Acreage Per Capita
If a census tract is designated as having access to a park, then it is assigned the functional acreage of that park. The acreage is summed for all the parks a census tract has access to. The total acreage is then divided by the census tract population to get acreage per capita (a rate). For larger parks, we do not assign the full acreage of the park to a census tract. The maximum amount assigned from a park is capped at 7,000,000 square feet or 160.6979 acres. This number is roughly equivalent to 0.25 square miles and is 3 standard deviations above the average acreage of all the parks in NYC.

#### Council District Aggregation
To analyze spatial equity at a legislative level, we aggregate the Census Tract data up to the 2023 City Council District (CD) level. Because Census Tracts do not nest cleanly into Council Districts, we perform a spatial join assigning each Census Tract centroid to a Council District.

To find the average park access experience for the whole district without artificially penalizing highly populated CDs, we calculate the population-weighted average of the local CT-level access rates. Mathematically, this simplifies to summing all accessible acres across a CD's constituent tracts and dividing by the sum of those tracts' populations: Sum(Accessible Acres) / Sum(CT Population). We also apply a population-weighted average to estimate CD-level Median Household Income. Additional demographic estimates for the CDs (such as Under 18 population, Over 65 population, and SNAP households) are sourced directly from the 2022 5-Year ACS using the councilcount package.

### Scripts

#### 01_load_dependencies.R
Loads necessary libraries and functions for use in the other scripts. 

#### 02_create_processed_data.Rmd
Creates the data found in the data/processed directory. Imports 2020 Census Tract shapefiles, pulls 2022 ACS data via the Census API (for CT-level population and median income), pulls 2022 ACS Council District demographic estimates via councilcount, and cleans the park maintenance data.

#### 03_park_cd_access.Rmd
Determines which open space access points are within 10 minutes walking from each census tract, assigns capped acreage, and performs the spatial join and population-weighted roll-up to the 2023 Council District level. Also calculates subgroup-specific per-capita rates.

#### 04_park_cd_map.Rmd
Creates interactive maps displaying park access, median income, and vulnerable population density at the Council District level.

#### 05_park_cd_corr.Rmd
Creates interactive scatterplots comparing park access to CD demographic percentages.


