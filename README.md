## Spatial-Analytics-Exercise-R

The purpose of this research project is to survey Chicago's socio-economic environment, 25 years after the 1995 heat wave --which resulted in about 514 heat-related deaths and 696 excess deaths primarily among low-income, African Americans-- for changes in structure against the backdrop of current population health statistics. Do current mortality rates follow the same patterns as observed in the 1995 heat wave, where "geography was linked to destiny"? If so, what can modern data tell us about people's living conditions and their capacity to deal with future disasters in the face of an ever-changing climate? 

🛠️ Tech Stack: R, sp, sf, tidyverse, leaflet, plotly, and more.
🌐 Objective: Analyze Chicago's socioeconomic factors & healthcare infrastructure.
💡 Highlights:

Merged multiple Chicago datasets, focusing on healthcare & socioeconomic trends.
Geo-visualized city metrics, revealing areas with shared attributes.
Identified clusters linking health outcomes with socioeconomic conditions.
---
## Data Source
The project revolves around datasets related to the city of Chicago. The datasets capture:

1. The locations (addresses) of hospitals in Chicago.
2. Data from the Chicago Data Portal.
---

## Libraries Used
1. **Spatial Data Handling:** `sp`, `sf`, `rgdal`, `rgeos`
2. **Data Wrangling:** `tidyverse`
3. **Data Visualization:** `tmap`, `leaflet`, `GGally`, `scatterplot3d`, `plotly`
4. **Statistics and Analytics:** `Hmisc`, `sjPlot`, `stargazer`, `corrplot`, `PerformanceAnalytics`

---
## Analysis
- Preliminary data exploration on both dependent and independent variables.
- Formatting and consolidation of data from various sources.
- Geo-visualizations of the data.
- Testing for local spatial autocorrelation within the dataset.
---

## Key Achievements
Clusters were identified in the scatter plot, which revealed shared attributes among socioeconomic variables. Notably, areas like Fuller Park, Grand Crossing, and Englewood, which were highlighted by Klinenberg (1999) as having experienced high heat-related deaths during the 1995 Chicago heatwave, still appear to be affected by poor socioeconomic conditions. These areas may also lack proper healthcare infrastructure in terms of access to nearby hospitals.

---

## Conclusion
The study indicates that clustering doesn't necessarily explain the reasons behind the observed patterns. Different processes can result in similar spatial patterns. Using regression analysis alongside novel geo-visualization techniques provides deeper insights into the community, its resources, and their effects on individual health and well-being. A significant linear association was observed between the percentage of individuals (aged 25 and older) without a high-school diploma and the percentage of households living in crowded conditions across various communities.

---

## Future Work
Potential future work could delve deeper into the underlying reasons for the observed spatial patterns, expand on the socio-economic factors contributing to health outcomes, or explore the impact of other infrastructural elements on community health.
