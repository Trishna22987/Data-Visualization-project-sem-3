**Project: Analysis of district-wise population density and road parameters in relation to road accidents (Feb–Jun 2024)**

This project explores the relationship between population density and road accidents at the district level in the UK. The study investigates how accident severity, road/weather conditions, and time-of-day patterns vary across districts with different population densities.
The project was completed by Trishna Chakraborty at Monash University.

🎯 **Objectives**

   1. Determine whether districts with higher population densities experience more road accidents.
   2. Compare variations in accident severity and conditions across different population density levels.
   3. Analyze accident distributions across days of the week and hours of the day.

**Dataset Sources**
  1. Road Accident Dataset (Kaggle)
  2. UK Population Dataset (ONS)  
  3. Geospatial Shapefiles (ONS) (Required for running geospatial analysis)
  4. Population Density Maps – ONS

👉 **Important: Please download the geospatial shapefile from the above link and save it in a folder named data/geospatial/ before running the R script.**

**🛠️ Tools & Libraries**
1.	Excel: Initial data inspection & cleaning
2.	R: Data wrangling, cleaning, visualization (ggplot2, dplyr, sf)
3.	Geospatial Mapping: District-level accident analysis, used leaflet and shiny to plot the geospatial results for visualisation.

**📑 Project Workflow**    
    
    ⦁	Data Cleaning & Wrangling
    
      1.	Removed duplicate/malformed accident IDs.
      2.	Corrected spelling errors in categorical fields.
      3.	Imputed missing values (NA/0).
      4.	Standardized date/time formats and column naming.
      5.	Merged accident data with population dataset.
      6.	Computed population density per district.
      7.	Integrated geospatial shapefile for mapping.
      8.	Did T-test to understand the compatibility of data.
    
    ⦁	Exploratory Analysis
    
      1.	Accident counts by time of day, day of week, month.
      2.	Severity analysis under different weather, road, and lighting conditions.
      3.	Heatmaps for speed zones and weather impacts.
      4.	Geospatial plots linking population density to accident frequency.
    
    ⦁	Key Findings
    
      1.	Higher population density districts generally have higher accident frequency.
      2.	Fridays and peak hours (3–8 PM) show higher accident risks.
      3.	Accidents occur mostly in moderate weather and speed conditions, emphasizing human/traffic factors.



Visualisations to understand the Correlations:
<img width="1427" height="926" alt="image" src="https://github.com/user-attachments/assets/9dde61d5-7d3c-443f-ac9f-75e704f87028" /> <img width="1752" height="971" alt="image" src="https://github.com/user-attachments/assets/5afd7782-50a5-4381-86cb-6ca468c026fe" />
<img width="1598" height="795" alt="image" src="https://github.com/user-attachments/assets/3d33897d-62ae-47e0-aaa7-13b31d4860c9" />
<img width="935" height="941" alt="image" src="https://github.com/user-attachments/assets/09148dbc-9df5-4af5-87fe-214d176f7ea2" />


