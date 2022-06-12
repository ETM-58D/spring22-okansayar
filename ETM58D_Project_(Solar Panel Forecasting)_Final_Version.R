
library(data.table)
library(ggplot2)
library(lubridate)
library(skimr)
library(dplyr)
library(lagged)
library(gridExtra)
library(GGally)
library(ggcorrplot)
library(Hmisc)
library(zoo)

production=fread("/Users/okansayar/Desktop/R/Project/production.csv")
weather=fread("/Users/okansayar/Desktop/R/Project/long_weather.csv")

head(production)
head(weather)

production[,timestamp:=as.Date(date)+dhours(hour)]
head(production)

ggplot(production,aes(x=timestamp,y=production)) + geom_line()
ggplot(production[date>'2022-04-25'],aes(x=timestamp,y=production)) + geom_line()

weather[,.N,list(lat,lon)]
weather[,.N,list(variable)]

dswrf=weather[variable=='DSWRF' & lat==36.5 & lon==33.25]
dswrf[,timestamp:=as.Date(date)+dhours(hour)]
head(dswrf)

ggplot(dswrf[date>'2022-04-25'],aes(x=timestamp,y=value)) + geom_line() + geom_line()

wide_weather=dcast(weather,date+hour~variable+lat+lon,value.var='value')
##head(wide_weather)
str(wide_weather)
str(production)

#3 Descriptive Analysis
skim(wide_weather)
solarprodhourly = production %>%
  group_by(hour) %>%
  summarise(
    count = n(),
    mean_production = mean(production),
    min_production = min(production),
    max_production = max(production)
  )
solarprodhourly

#Histogram shows that hours between 05:00 and 19:00 that only time for production.
ggplot(solarprodhourly, aes(x=hour, y=mean_production)) + geom_bar(stat = "identity")

# Time period of between 19 PM to 05 AM (next day), there is not any production. 
# So, ı cast out these period for only focusing day time.
daytime <- solarprodhourly %>% filter(hour >= 5 & hour <= 19)
head(daytime, 20)

#Histogram shows that avg cloud layer on different months
wide_weather$AVG_CLOUD_LOW_LAYER=((wide_weather$CLOUD_LOW_LAYER_36.25_33+
                        
  wide_weather$CLOUD_LOW_LAYER_36.25_33.25+wide_weather$CLOUD_LOW_LAYER_36.25_33.5+
  wide_weather$CLOUD_LOW_LAYER_36.5_33+wide_weather$CLOUD_LOW_LAYER_36.5_33.25+
  wide_weather$CLOUD_LOW_LAYER_36.5_33.5+wide_weather$CLOUD_LOW_LAYER_36.75_33+
  wide_weather$CLOUD_LOW_LAYER_36.75_33.25+wide_weather$CLOUD_LOW_LAYER_36.75_33.5)/9)

G1=ggplot(wide_weather, aes(x=date, y=AVG_CLOUD_LOW_LAYER)) + geom_line() + ggtitle("Cloud layer in regard to months")


wide_weather$AVG_TEMP=((wide_weather$TEMP_36.25_33+
  wide_weather$TEMP_36.25_33.25+wide_weather$TEMP_36.25_33.5+
  wide_weather$TEMP_36.5_33+wide_weather$TEMP_36.5_33.25+
  wide_weather$TEMP_36.5_33.5+wide_weather$TEMP_36.75_33+
  wide_weather$TEMP_36.75_33.25+wide_weather$TEMP_36.75_33.5)/9)
                        
G2=ggplot(wide_weather, aes(x=date, y=AVG_TEMP)) + geom_line() + ggtitle("Temperature in regard to months")

wide_weather$AVG_REL_HUMIDITY=((wide_weather$REL_HUMIDITY_36.25_33+
  wide_weather$REL_HUMIDITY_36.25_33.25+wide_weather$REL_HUMIDITY_36.25_33.5+
  wide_weather$REL_HUMIDITY_36.5_33+wide_weather$REL_HUMIDITY_36.5_33.25+
  wide_weather$REL_HUMIDITY_36.5_33.5+wide_weather$REL_HUMIDITY_36.75_33+
  wide_weather$REL_HUMIDITY_36.75_33.25+wide_weather$REL_HUMIDITY_36.75_33.5)/9)

G3=ggplot(wide_weather, aes(x=date, y=AVG_REL_HUMIDITY)) + geom_line() + ggtitle("Humidity in regard to months")

wide_weather$AVG_DSWRF=((wide_weather$DSWRF_36.25_33+
              wide_weather$DSWRF_36.25_33.25+wide_weather$DSWRF_36.25_33.5+
              wide_weather$DSWRF_36.5_33+wide_weather$DSWRF_36.5_33.25+
              wide_weather$DSWRF_36.5_33.5+wide_weather$DSWRF_36.75_33+
              wide_weather$DSWRF_36.75_33.25+wide_weather$DSWRF_36.75_33.5)/9)

G4=ggplot(wide_weather, aes(x=date, y=AVG_DSWRF)) + geom_line() + ggtitle("DSWRF in regard to months")


# Summer season cloud low layer decrease radically especially on between April-October
# Avg temperature has a similar behavior like cloud low layer. 
# While summer season, temperature going up. 
# While DSWRF is highly correlated with temperature. From April to October we seen a higher temperature & DSWRF 
# Humidity is also reverse relationship with Temperature and DSWRF. So in summer season, we seen a lower humidity level. 
# So, these figures highly correlated each other so taking their avg values help us to decrease variables numbers.

grid.arrange(G1, G2, ncol=2)
grid.arrange(G3, G4, ncol=2)

weathernew = wide_weather[,-(3:38)]
head(data.table(weathernew))

#merge two data sets in a one (combine weather.csv and production.csv)
merged_production_weather <- full_join(weathernew, production)
merged_production_weather

# Relationship between cloud low layer. So cloud low layer increase, production is decreasing based on graph
plot(merged_production_weather$AVG_CLOUD_LOW_LAYER, merged_production_weather$production, xlab = "Cloud Low Layer", ylab = "Production", main = "Cloud Low Layer-Production Scatter Plot")
# Relationship between DSWRF between production. While DSWRF is increase, production is also increased based on graph
plot(merged_production_weather$AVG_DSWRF, merged_production_weather$production, xlab = "DSWRF", ylab = "Production", main = "DSWRF-Production Scatter Plot")
# Relationship between DSWRF & Hour. DSWRF is directly correlated with daytime and sunlight. 
# Mid-time of day is highest DSWRF values. 
plot(merged_production_weather$hour,merged_production_weather$AVG_DSWRF, xlab = "Hour", ylab = "DSWRF", main = "DSWRF-Hour Scatter Plot")
# While temperature and DSWRF is highly correlated. One of them increase led to increase another one.
plot(merged_production_weather$AVG_DSWRF,merged_production_weather$AVG_TEMP, xlab = "DSWRF", ylab = "Temperature", main = "DSWRF-Temperature Scatter Plot")
# While cloud low layer increase do not change the temperature or vice versa. So, temperature values stay at same level.
plot(merged_production_weather$AVG_CLOUD_LOW_LAYER,merged_production_weather$AVG_TEMP, xlab = "Cloud Low Layer", ylab = "Temperature", main = "Cloud Low Layer-Temperature Scatter Plot")

head(data.table(merged_production_weather))
## Lag arranged different time of day, fromm 1 to 168 hour with different time series like (1 Hour, 6 Hour, 12 Hour, 24 Hour, 48 Hour, 168 Hour (one week))
## 1 Hour Lag
merged_production_weather$Production1hourlag  = lag(merged_production_weather$production, 1)
merged_production_weather$Cloud1hourlag  = lag(merged_production_weather$AVG_CLOUD_LOW_LAYER, 1)
merged_production_weather$Dswrf1hourlag  = lag(merged_production_weather$AVG_DSWRF, 1)
merged_production_weather$Temp1hourlag  = lag(merged_production_weather$AVG_TEMP, 1)
merged_production_weather$Humidity1hourlag  = lag(merged_production_weather$AVG_REL_HUMIDITY, 1)

## 6 Hour Lag
merged_production_weather$Production6hourlag  = lag(merged_production_weather$production, 6)
merged_production_weather$Cloud6hourlag  = lag(merged_production_weather$AVG_CLOUD_LOW_LAYER, 6)
merged_production_weather$Dswrf6hourlag  = lag(merged_production_weather$AVG_DSWRF, 6)
merged_production_weather$Temp6hourlag  = lag(merged_production_weather$AVG_TEMP, 6)
merged_production_weather$Humidity6hourlag  = lag(merged_production_weather$AVG_REL_HUMIDITY, 6)

## 12 Hour Lag
merged_production_weather$Production12hourlag  = lag(merged_production_weather$production, 12)
merged_production_weather$Cloud12hourlag  = lag(merged_production_weather$AVG_CLOUD_LOW_LAYER, 12)
merged_production_weather$Dswrf12hourlag  = lag(merged_production_weather$AVG_DSWRF, 12)
merged_production_weather$Temp12hourlag  = lag(merged_production_weather$AVG_TEMP, 12)
merged_production_weather$Humidity12hourlag  = lag(merged_production_weather$AVG_REL_HUMIDITY, 12)


## 24 Hour Lag
merged_production_weather$Production24hourlag  = lag(merged_production_weather$production, 24)
merged_production_weather$Cloud24hourlag  = lag(merged_production_weather$AVG_CLOUD_LOW_LAYER, 24)
merged_production_weather$Dswrf24hourlag  = lag(merged_production_weather$AVG_DSWRF, 24)
merged_production_weather$Temp24hourlag  = lag(merged_production_weather$AVG_TEMP, 24)
merged_production_weather$Humidity24hourlag  = lag(merged_production_weather$AVG_REL_HUMIDITY, 24)

## 48 Hour Lag
merged_production_weather$Production48hourlag  = lag(merged_production_weather$production, 48)
merged_production_weather$Cloud48hourlag  = lag(merged_production_weather$AVG_CLOUD_LOW_LAYER, 48)
merged_production_weather$Dswrf48hourlag  = lag(merged_production_weather$AVG_DSWRF, 48)
merged_production_weather$Temp48hourlag  = lag(merged_production_weather$AVG_TEMP, 48)
merged_production_weather$Humidity48hourlag  = lag(merged_production_weather$AVG_REL_HUMIDITY, 48)

## 168 Hour Lag
merged_production_weather$Production168hourlag  = lag(merged_production_weather$production, 168)
merged_production_weather$Cloud168hourlag  = lag(merged_production_weather$AVG_CLOUD_LOW_LAYER, 168)
merged_production_weather$Dswrf168hourlag  = lag(merged_production_weather$AVG_DSWRF, 168)
merged_production_weather$Temp168hourlag  = lag(merged_production_weather$AVG_TEMP, 168)
merged_production_weather$Humidity168hourlag  = lag(merged_production_weather$AVG_REL_HUMIDITY, 168)


merged_production_weather$Production3hourMA  = rollmean(merged_production_weather$AVG_TEMP, 3,na.pad = T, align = "right")
merged_production_weather$Cloud3hourMA  = rollmean(merged_production_weather$AVG_TEMP, 3,na.pad = T, align = "right")
merged_production_weather$Dswrf3hourMA  = rollmean(merged_production_weather$AVG_TEMP, 3,na.pad = T, align = "right")
merged_production_weather$Temp3hourMA  = rollmean(merged_production_weather$AVG_TEMP, 3,na.pad = T, align = "right")

merged_production_weather$Production24hourMA  = rollmean(merged_production_weather$AVG_TEMP, 24,na.pad = T, align = "right")
merged_production_weather$Cloud24hourMA  = rollmean(merged_production_weather$AVG_TEMP, 24,na.pad = T, align = "right")
merged_production_weather$Dswrf24hourMA  = rollmean(merged_production_weather$AVG_TEMP, 24,na.pad = T, align = "right")
merged_production_weather$Temp24hourMA  = rollmean(merged_production_weather$AVG_TEMP, 24,na.pad = T, align = "right")

merged_production_weather$Production48hourMA  = rollmean(merged_production_weather$AVG_TEMP, 48,na.pad = T, align = "right")
merged_production_weather$Cloud48hourMA  = rollmean(merged_production_weather$AVG_TEMP, 48,na.pad = T, align = "right")
merged_production_weather$Dswrf48hourMA  = rollmean(merged_production_weather$AVG_TEMP, 48,na.pad = T, align = "right")
merged_production_weather$Temp48hourMA  = rollmean(merged_production_weather$AVG_TEMP, 48,na.pad = T, align = "right")

merged_production_weather$Production168hourMA  = rollmean(merged_production_weather$AVG_TEMP, 168,na.pad = T, align = "right")
merged_production_weather$Cloud168hourMA  = rollmean(merged_production_weather$AVG_TEMP, 168,na.pad = T, align = "right")
merged_production_weather$Dswrf168hourMA  = rollmean(merged_production_weather$AVG_TEMP, 168,na.pad = T, align = "right")
merged_production_weather$Temp168hourMA  = rollmean(merged_production_weather$AVG_TEMP, 168,na.pad = T, align = "right")

merged_production_weather = merged_production_weather[complete.cases(merged_production_weather), ]

##head(data.table(merged_production_weather))
skim(merged_production_weather)

##Partitioning
merged_new = merged_production_weather[,-c(1,3,4,5,6,8)]
set.seed(123)
merged_new_train =  merged_new[1:(nrow(merged_new)-(7*24+1)),]
merged_new_test = merged_new[(nrow(merged_new)-(7*24)):nrow(merged_new),]
##head(data.table(merged_new_train))

# Approach to problem:
# In this problem, firstly descriptive analysis method is used to see the relationship between production amount and the other features. The data was tuned by taking the average of the values given for the coordinates that are close to each other. Lags were calculated every 6 hours and weekly, and the dataset was pre-processed with the results and prepared for the estimation process. Then, hourly forecasts were made by creating train and test sets. Using the train set, 6 different regression models were established with the linear regression method and predictions were made. After a regression model, the one with the greatest effect from the independent variables was controlled and basically those variables were used in the next model. RMSE and R^2 were used as accuracy measure from the statistical results. The model with the highest R^2 value was determined to be used in the predictions.

model1<-lm(production ~ . ,data = merged_new_train)
print(summary(model1))
predicttest= predict(model1, merged_new_test)
model1comparison=data.frame(hour = merged_new_test$hour, realTestProduction=merged_new_test$production, predictionTestProduction=predicttest)
print(head(model1comparison,24))
library(tidyverse)
library(caret)
RMSE(predicttest, merged_new_test$production) 
R1=R2(predicttest, merged_new_test$production) #0.90 güzel sonuç

model2<-lm(production ~ Production1hourlag + Cloud1hourlag + Dswrf1hourlag + Temp1hourlag ,data = merged_new_train)
print(summary(model2))
predicttest = predict(model2, merged_new_test)
model2comparison= data.frame(hour = merged_new_test$hour, realTestproduction=merged_new_test$production, predictionTestproduction=predicttest)
print(head(model2comparison,24))
RMSE(predicttest, merged_new_test$production)
R2=R2(predicttest, merged_new_test$production)

model3<-lm(production ~ Production1hourlag + Cloud1hourlag + Dswrf1hourlag + Temp1hourlag + Production6hourlag + Cloud6hourlag + Dswrf6hourlag + Temp6hourlag,data = merged_new_train)
print(summary(model3))
predicttest = predict(model3, merged_new_test)
model3comparison= data.frame(hour = merged_new_test$hour, realTestproduction=merged_new_test$production, predictionTestproduction=predicttest)
print(head(model3comparison,24))
RMSE(predicttest, merged_new_test$production)
R3=R2(predicttest, merged_new_test$production)

model4<-lm(production ~ Production1hourlag + Cloud1hourlag + Dswrf1hourlag + Temp1hourlag + Production12hourlag + Cloud12hourlag + Dswrf12hourlag + Temp12hourlag + Production24hourlag + Cloud24hourlag + Dswrf24hourlag + Temp24hourlag + Production48hourlag + Cloud48hourlag + Dswrf48hourlag + Temp48hourlag ,data = merged_new_train)
print(summary(model4))
predicttest = predict(model4, merged_new_test)
model4comparison= data.frame(hour = merged_new_test$hour, realTestproduction=merged_new_test$production, predictionTestproduction=predicttest)
print(head(model4comparison,24))
RMSE(predicttest, merged_new_test$production)
R4=R2(predicttest, merged_new_test$production)


model5<- lm(production ~ Production1hourlag + Cloud1hourlag + Dswrf1hourlag + Temp1hourlag + Production6hourlag + Cloud6hourlag + Dswrf6hourlag + Temp6hourlag + Production24hourlag + Cloud24hourlag + Dswrf24hourlag + Temp24hourlag + Production168hourlag + Cloud168hourlag + Dswrf168hourlag + Temp168hourlag ,data = merged_new_train)
print(summary(model5))
predicttest = predict(model4, merged_new_test)
model5comparison= data.frame(hour = merged_new_test$hour, realTestproduction=merged_new_test$production, predictionTestproduction=predicttest)
print(head(model5comparison,24))
RMSE(predicttest, merged_new_test$production)
R5=R2(predicttest, merged_new_test$production)

model6<- lm(production ~ Production1hourlag + Cloud1hourlag + Dswrf1hourlag + Temp1hourlag + Production12hourlag + Cloud12hourlag + Dswrf12hourlag + Temp12hourlag + Production48hourlag + Cloud48hourlag + Dswrf48hourlag + Temp48hourlag + Production168hourlag + Cloud168hourlag + Dswrf168hourlag + Temp168hourlag ,data = merged_new_train)
print(summary(model6))
predicttest = predict(model5, merged_new_test)
model6comparison= data.frame(hour = merged_new_test$hour, realTestproduction=merged_new_test$production, predictionTestproduction=predicttest)
print(head(model6comparison,24))
RMSE(predicttest, merged_new_test$production)
R6=R2(predicttest, merged_new_test$production)

cbind(c(R1 = R1, R2 = R2, R3 = R3, 
        R4 = R4, R5 = R5, R6 = R6))
print("Model 1 is the best among all 6 models")
summary(R1)

# Results & Conclusions: 

# When we build our model in a 6 different type; although there is an exceptional and outliers production data for each 6 models, average prediction and actual results are very close to each other.Due to numerous production data between day time (06:00 - 19:00) our estimation models work better. In every prediction our method and estimation systems works over %85 accurately.
# As a results, variables which is located in a different places are similar to each other and has a same output information.And we can say that these variables high correlated to each others.So, in this we're calculated average values for 9 different locations on; Temperature ,Cloud Low Layer, Humidity and DSWRF values for using one variable to each attributes. In building 6 different prediction model for hourly estimation; we used R squared (R2) method which is is a regression error metric that justifies the performance of the model. So, when we calculate the R2 prediction; Model 1 makes prediction with %90 percent accuracy so that's why we used Model 1 is the best one.

