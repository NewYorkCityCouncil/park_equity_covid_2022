## Park Equity & COVID-19
Data analysis and visuals for NYCC 4.22.22 ['Oversight: The Effect of COVID-19 on Park Equity'](https://legistar.council.nyc.gov/MeetingDetail.aspx?ID=951908&GUID=4D8FDC0B-C36E-4C0D-9346-392A41B04110&Options=info|&Search=) hearing.

***  

### Data Sources 
- [Walk-to-a-Park Service Area](https://data.cityofnewyork.us/Recreation/Walk-to-a-Park-Service-area/5vb5-y6cv)
- [Parks Maintenance Report](https://www.nycgovparks.org/pagefiles/173/Fiscal-Year-2021-Annual-Report-on-Park-Maintenance__61e6f4a01b623.pdf)
- [2010 Census Tracts](https://www1.nyc.gov/site/planning/data-maps/open-data/census-download-metadata.page)
- [2010 ZCTA to Census Tract Relationship File](https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt)
- [NYC DOHMH Geographic Resources](https://github.com/nychealth/coronavirus-data/tree/master/Geography-resources)
- 2019 5-Year ACS Survey: *We used R package censusapi to get demographic data

### Methodology 

#### Summary & Intention
- Calculate how much functional acreage of park space residents of NYC have access to.
- Analyze geographic & income disparities in access to park space & the relation to COVID cases.


#### Parks & Open Spaces included in Analysis
From the [Annual Report on Park Maintenance (Local Law 98 of 2015)](https://www.nycgovparks.org/news/archive)

#### 10 Minute Walking Distance
Using the access points from Walk-to-a-Park Service Area dataset, we created isochrone polygons or time-distance areas for each point. We used mapbox api for this process and selected a 10 minute walking distance parameter. If the center of a census tract is within 10-minutes walking of any access point associated with a given park, then it is designated as having access to that park. 

#### Square Feet Per Capita
If a census tract is designated as having access to a park, then it is assigned the functional acreage of that park. The acreage is summed for all the parks a census tract has access to. The total acreage is then divided by the census tract population to get acreage per capita. For larger parks, we do not assign the full acreage of the park to a census tract. The maximum amount assigned from a park is capped at 7,000,000 square feet or 160.6979 acres. This number is roughly equivalent to 0.25 square miles and is 3 standard deviations above the average acreage of all the parks in NYC. 

#### Zip Code Aggregation
To compare the COVID-19 data to our open space access data, we aggregate the census tracts up to the MODZCTA level. Census tract data is first aggregated to ZCTA5 level using the [Census crosswalk relationship file](https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt) and doing a population-weighted assignment of acreage. Refer to the [technical document](https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2010-zcta-record-layout.html#par_textimage_3) for more information on the crosswalk.  

The data is then aggregated to the MODZCTA level using the [NYC DOHMH files](https://github.com/nychealth/coronavirus-data/tree/master/Geography-resources). The MODZCTA acreage is a population-weighted average of each nested ZCTA value. 


### Scripts

#### load_dependencies.R
Loads necessary libraries and functions for use in the other scripts. 

#### create_processed_data.R
Creates the data found in [data/processed](https://github.com/NewYorkCityCouncil/park_equity_covid_2022/tree/main/data/processed). Imports and cleans park maintenance data, imports ACS data, joins and writes shapefiles with ACS data.  

#### park_ct.R
This script determines which open space access points are within 10 minutes walking from each census tract and assigns acreage per capita, demographic variables, and park maintenance variables. 

#### park_modzcta.R
This script aggregates the data up to the zip code (modzcta) level. 

#### park_modzcta_map.R
Creates maps at the zip code (modzcta level). 

#### park_modzcta_corr.R
Creates plots of correlations at the zip code (modzcta level). 


