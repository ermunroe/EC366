library(readr)
library(tidyverse)


#The fable package is recommended for time series forecasting
library(fable)
library(tsibble)


#This section graphs forecasts and actual data at the DAILY level

preCovidData = read.csv("preCovidData")
preCovidData = preCovidData %>% mutate(date_local = as_date(date_local))
preCovidData = as_tsibble(preCovidData,key = county_code,index=date_local)
preCovidData = tsibble::fill_gaps(preCovidData)
m = preCovidData %>% model(arima = ARIMA(aqi))
testingForecast = m %>% forecast(h = 90,frequency =1)

postCovidData = read.csv("postCovidData")
postCovidData = postCovidData %>% mutate(date_local = as_date(date_local))
postCovidData = postCovidData %>% filter(date_local < as_date("2021-01-01"))
postCovidData = as_tsibble(postCovidData,key = county_code,index=date_local)

preCovidData = read.csv("preCovidData")
preCovidData = preCovidData %>% mutate(date_local = as_date(date_local))
preCovidData = as_tsibble(preCovidData,key = county_code,index=date_local)
preCovidData = preCovidData %>% filter(date_local > as_date("2020-01-01"))

countyCodeVec = c(1,5,13,17,25,39,47,61,75,81,101)
for (i in countyCodeVec){
  tempPreData = preCovidData %>% filter(county_code == i)
  tempPreData = tempPreData %>% select(date_local,aqi)
  
  
  tempPostData = postCovidData %>% filter(county_code == i)
  tempPostData = tempPostData %>% select(date_local,aqi)
  
  print(testingForecast %>% filter(county_code==i) %>% autoplot() + autolayer(tempPostData)+ autolayer(tempPreData))
}

#This section graphs forecasts and actual data, aggregated at the MONTHLY level
#Jesse's recommendations:
#Aggregated monthy, trained on data beginning march 2019
preCovidData = read.csv("preCovidData")
preCovidData = preCovidData %>% mutate(date_local = as_date(date_local))
preCovidData = preCovidData %>% filter(date_local >= as_date("2019-03-01"))
preCovidData <- preCovidData %>% mutate(month = format(date_local, "%Y-%m"))
preCovidData <- preCovidData %>% group_by(state_code, county_code, month) %>% summarize(aqi = mean(aqi, na.rm = TRUE)) %>% ungroup()
preCovidData <- preCovidData %>% mutate(month = ym(month))
preCovidData = as_tsibble(preCovidData,key = county_code,index=month)
preCovidData = tsibble::fill_gaps(preCovidData)
m = preCovidData %>% model(arima = ARIMA(aqi))
testingForecast = m %>% forecast(h = 90,frequency =1)

postCovidData = read.csv("postCovidData")
postCovidData = postCovidData %>% mutate(date_local = as_date(date_local))
postCovidData <- postCovidData %>% mutate(month = format(date_local, "%Y-%m"))
postCovidData <- postCovidData %>% group_by(state_code, county_code, month) %>% summarize(aqi = mean(aqi, na.rm = TRUE)) %>% ungroup()
postCovidData <- postCovidData %>% mutate(month = ym(month))
postCovidData = postCovidData %>% filter(month < as_date("2022-01-01"))
postCovidData = as_tsibble(postCovidData,key = county_code,index=month)

preCovidData = read.csv("preCovidData")
preCovidData = preCovidData %>% mutate(date_local = as_date(date_local))
preCovidData <- preCovidData %>% mutate(month = format(date_local, "%Y-%m"))
preCovidData <- preCovidData %>% group_by(state_code, county_code, month) %>% summarize(aqi = mean(aqi, na.rm = TRUE)) %>% ungroup()
preCovidData <- preCovidData %>% mutate(month = ym(month))
preCovidData = as_tsibble(preCovidData,key = county_code,index=month)
preCovidData = preCovidData %>% filter(month > as_date("2019-01-01"))




countyCodeVec = c(1,5,13,17,25,39,47,61,75,81,101)
for (i in countyCodeVec){
  tempPreData = preCovidData %>% filter(county_code == i)
  tempPreData = tempPreData %>% select(month,aqi)
  
  tempPostData = postCovidData %>% filter(county_code == i)
  tempPostData = tempPostData %>% select(month,aqi)
  
 # tempFull = fullData %>% filter(county_code == i)
 # tempFull = tempFull %>% select(month,aqi)
  
 print(testingForecast %>% filter(county_code==i) %>% autoplot() + autolayer(tempPostData)+ autolayer(tempPreData))
#print(testingForecast %>% filter(county_code==i) %>% autoplot() + autolayer(tempFull))
}


#This section performs a regression, a la The Effect (a la Effect? I guess that's an extra "the") section 17.3
preCovidData = read.csv("preCovidData")
preCovidData = preCovidData %>% mutate(date_local = as_date(date_local))
preCovidData = preCovidData %>% filter(date_local >= as_date("2019-03-01"))

postCovidData = read.csv("postCovidData")
postCovidData = postCovidData %>% mutate(date_local = as_date(date_local))
postCovidData = postCovidData %>% filter(date_local < as_date("2022-01-01"))

fullData = rbind(preCovidData,postCovidData)
fullData = fullData %>% unique()
fullData = fullData %>% mutate(month = format(date_local, "%Y-%m"))
fullData <- fullData %>% group_by(state_code, county_code, month) %>% summarize(aqi = mean(aqi, na.rm = TRUE))



library(fixest)
m <- feols(aqi ~ i(month, ref = "2020-03"), data = fullData, cluster = 'county_code')

coefplot(m, drop = c('(Intercept)',"Constant"),
         pt.join = TRUE, ref = c('month:2020-03' = 14), ref.line = TRUE)





