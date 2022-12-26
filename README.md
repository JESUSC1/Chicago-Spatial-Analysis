## Introduction to Spatial Data Science Course Project, Programming Language: R, Tools: GeoDa, Duration: 4 Weeks

The purpose of this research project is to survey Chicago's socio-economic environment, 25 years after the 1995 heat wave --which resulted in about 514 heat-related deaths and 696 excess deaths primarily among low-income, African Americans-- for changes in structure against the backdrop of current population health statistics. Do current mortality rates follow the same patterns as observed in the 1995 heat wave, where "geography was linked to destiny"? If so, what can modern data tell us about people's living conditions and their capacity to deal with future disasters in the face of an ever-changing climate? 

• Gained familiarity creating geo-visualizations and conducting spatial analysis in R
• Implemented spatial autocorrelation analysis (e.g., Anselin Local Moran’s I) to identify clusters— locations of statistically significant hot spots, cold spots, and spatial outliers— using GeoDa, a software for spatial modeling
• Gained experience initiating and driving projects to completion with minimal guidance


## Final Project Guidelines
Luc Anselin Revised 09/15/2019

The final project should pull together a range of techniques covered in the class to carry out a spatial data exploration. Ideally, this should result in the identification of potential hypotheses or relationships, or lead to discovering the unexpected.
Choose any data set, but make sure you have at least 50 observations. The data can be points, but only if you can analyze the points as discrete units of observation, NOT if the points represent events (such as crimes, accidents, pot holes). In the latter case, you must aggregate the data to an areal unit (such as a community area, census tract, county). You should have at least three variables (more is better).
You do not have to use each and every technique covered in class, but you must include:

• EDA to visualize the non-spatial aspects of the data and explore relationships between variables.
• Geo-visualization (mapping) to describe the spatial aspects of the data and to assess potential spatial heterogeneity (or other structure).
• Local spatial autocorrelation analysis to detect clusters (with a sensitivity analysis for the choice of weights, p-values, etc.).
• You may want to reduce multiple explanatory variables (such as census data) to their main principal components (optional).
Your write-up (not including tables and figures) should be 5-10pp. The focus is on the analysis, not on literature reviews, theoretical motivation, etc. 
