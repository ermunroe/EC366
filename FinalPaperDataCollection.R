library(readr)
library(tidyverse)

#They recommend you install the Keyring package to help manage the key
install.packages("keyring")
keyring::has_keyring_support()
library(keyring) 
key_set(service = "AQSDatamart", username = "ermunroe@gmail.com")

#Key to access the EPA air quality API
#Jesse, if you see this, please don't launch an attack on their API with our emails and get us banned
#goldwren18

#Setting up 
library(RAQSAPI)
datamartAPI_user <- "ermunroe@gmail.com" 
server <- "AQSDatamart"


aqs_credentials(username = datamartAPI_user, key = key_get(service = server, username = datamartAPI_user ) )


##This is the preCovidData collection process

#Particulate Matter 2.5 (PM2.5): Parameter code often represented as "88101".

#New York County
dta = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-23"), stateFIPS = "36", countycode = "061")

#Kings County
dta2 = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-23"), stateFIPS = "36", countycode = "047")

#Bronx
dta3 = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-23"), stateFIPS = "36", countycode = "005")

#Queens
dta4 = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-23"), stateFIPS = "36", countycode = "081")

#San Francisco
dta5 = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-19"), stateFIPS = "06", countycode = "075")

#Arlington County
dta6 = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-30"), stateFIPS = "51", countycode = "013")

#Hudson County
dta7 = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-16"), stateFIPS = "34", countycode = "017")

#Alexandria County
dta8 = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-30"), stateFIPS = "51", countycode = "510")

#Union County New Jersey
dta9 = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-16"), stateFIPS = "34", countycode = "039")

#Suffolk County - DATE WAS 2019
dta10 = aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-15"), stateFIPS = "25", countycode = "025")

#Philadelphia County
dtaPhil <- aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-23"), stateFIPS = "42", countycode = "101")

#DC County
dtaDC <- aqs_dailysummary_by_county("88101",as.Date("2015-01-01"),as.Date("2020-03-16"), stateFIPS = "11", countycode = "001")
dta <- rbind(dta,dta2,dta3,dta4,dta5,dta6,dta7,dta8,dta9,dta10, dtaPhil, dtaDC)

dta <- dta[,c('state_code', 'county_code', 'site_number', 'aqi', 'date_local', 'arithmetic_mean')]
library(dplyr)
dta <- dta %>% group_by(state_code, county_code, date_local) %>% summarize(aqi = mean(aqi, na.rm = TRUE)) %>% ungroup()

write.csv(dta, file="preCovidData", row.names=FALSE)


#Post COVID data
#New York County
dta = aqs_dailysummary_by_county("88101",as.Date("2020-03-23"),as.Date("2024-05-01"), stateFIPS = "36", countycode = "061")

#Kings County
dta2 = aqs_dailysummary_by_county("88101",as.Date("2020-03-23"),as.Date("2024-05-01"), stateFIPS = "36", countycode = "047")

#Bronx
dta3 = aqs_dailysummary_by_county("88101",as.Date("2020-03-23"),as.Date("2024-05-01"), stateFIPS = "36", countycode = "005")

#Queens
dta4 = aqs_dailysummary_by_county("88101",as.Date("2020-03-23"),as.Date("2024-05-01"), stateFIPS = "36", countycode = "081")

#San Francisco
dta5 = aqs_dailysummary_by_county("88101",as.Date("2020-03-19"),as.Date("2024-05-01"), stateFIPS = "06", countycode = "075")

#Arlington County
dta6 = aqs_dailysummary_by_county("88101",as.Date("2020-03-30"),as.Date("2024-05-01"), stateFIPS = "51", countycode = "013")

#Hudson County
dta7 = aqs_dailysummary_by_county("88101",as.Date("2020-03-16"),as.Date("2024-05-01"), stateFIPS = "34", countycode = "017")

#Alexandria County
dta8 = aqs_dailysummary_by_county("88101",as.Date("2020-03-30"),as.Date("2024-05-01"), stateFIPS = "51", countycode = "510")

#Union County New Jersey
dta9 = aqs_dailysummary_by_county("88101",as.Date("2020-03-16"),as.Date("2024-05-01"), stateFIPS = "34", countycode = "039")

#Suffolk County - DATE WAS 2019
dta10 = aqs_dailysummary_by_county("88101",as.Date("2020-03-15"),as.Date("2024-05-01"), stateFIPS = "25", countycode = "025")

#Philadelphia County
dtaPhil <- aqs_dailysummary_by_county("88101",as.Date("2020-03-23"),as.Date("2024-05-01"), stateFIPS = "42", countycode = "101")

#DC County
dtaDC <- aqs_dailysummary_by_county("88101",as.Date("2020-03-16"),as.Date("2024-05-01"), stateFIPS = "11", countycode = "001")
dta <- rbind(dta,dta2,dta3,dta4,dta5,dta6,dta7,dta8,dta9,dta10, dtaPhil, dtaDC)

dta <- dta[,c('state_code', 'county_code', 'site_number', 'aqi', 'date_local', 'arithmetic_mean')]
library(dplyr)
dta <- dta %>% group_by(state_code, county_code, date_local) %>% summarize(aqi = mean(aqi, na.rm = TRUE)) %>% ungroup()

write.csv(dta, file="postCovidData", row.names=FALSE)
