# EPFRs-and-OP-in-SDS_Hospitalization

#DATA
The air quality data was available from China National Environmental Monitoring Centre and National Urban Air Quality Real-Time Distribution Platform (https://www.cnemc.cn/) upon request. The exposure data for meteorological data in this study were downloaded from National Climatic Data Center of the United States (https://www.ncei.noaa.gov/). For health outcome data from the cause-specific hospitalization, the authors are not permitted to share the raw data. Researchers who are interested should contact the data provider via http://www.phic.org.cn/.

#R CODE
In the Main_Code/Random_Forest.R code, we use the randomForest method in R to model and predict EPFRS and DDT data.
In Main_Code/Exposure_Response_Association.R, we use the results of random forest prediction, combined with hospitalization data, through the mvmeta and glm models in R language, to analyze the associations between short-term EPFRs or OP exposure and hospitalizations during SDS periods.
