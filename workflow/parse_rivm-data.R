require(tidyverse)
require(data.table)
rm(list=ls())

rivm.data <- read.csv("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv", sep=";") ## Read in data with all cases until today
filename <- paste0("data-rivm/casus-datasets/COVID-19_casus_landelijk_",Sys.Date(),".csv")

paste0("data-rivm/casus-data/COVID-19_casus_landelijk_",Sys.Date(),".csv")
write.csv(rivm.data, file=filename) ## Write file with all cases until today

rivm.death <- rivm.data %>%
  dplyr::filter(Deceased == "Yes") ## Extract deaths data only

rivm.hospital <- rivm.data %>%
  dplyr::filter(Hospital_admission == "Yes") ## Extract hospital data only

rivm.dailydata <- data.frame(as.Date(Sys.Date()),nrow(rivm.data),nrow(rivm.hospital),nrow(rivm.death)) ## Calculate totals for cases, hospitalizations, deaths
names(rivm.dailydata) <- c("date","cases","hospitalization","deaths")

filename.daily <- paste0("data-rivm/data-per-day/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
write.csv(rivm.dailydata, file = filename.daily) ## Write file with daily data

rivm.daily_aggregate <- read.csv("data/rivm_by_day.csv") ## Read in aggregate data
rivm.daily_aggregate <- rivm.daily_aggregate[,-c(1,6)] ## Remove identifier column

rivm.daily_aggregate <- rbind(rivm.dailydata, rivm.daily_aggregate) ## Bind data today with aggregate data per day
rivm.daily_aggregate <- rivm.daily_aggregate[order(rivm.daily_aggregate$date),]
rivm.daily_aggregate$date <- as.Date(rivm.daily_aggregate$date)
rivm.daily_aggregate <- rivm.daily_aggregate %>%
  mutate(positivetests = c(0,diff(cases))) # Calculate number of positive tests per day

write.csv(rivm.daily_aggregate, file = "data/rivm_by_day.csv") ## Write file with aggregate data per day

## Data for municipalities

rivm.municipalities <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv", sep=";")
filename.municipality <- paste0("data-rivm/municipal-datasets/rivm_municipality_",Sys.Date(),".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities, file=filename.municipality)

rm(list=ls())

