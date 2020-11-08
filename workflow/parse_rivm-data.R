require(cowplot)
require(tidyverse)
require(rjson)
require(data.table)

rm(list=ls())

# Data municipalities per day
rivm.mun.perday <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv", sep=";")
sum(rivm.mun.perday$Total_reported)-404401
last_date <- as.Date(last(rivm.mun.perday$Date_of_report))
filename.mun.perday <- paste0("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_", last_date, ".csv") ## Filename for daily data municipalities
write.csv(rivm.mun.perday, file=filename.mun.perday,row.names = F)

rivm.mun.cum <- rivm.mun.perday %>%
  group_by(
    Municipality_code, 
    Security_region_code, 
    ROAZ_region, 
    Province
  ) %>%
  mutate(
    Total_reported_cum = cumsum(Total_reported),
    .after = Total_reported
  ) %>%
  mutate(
    Hospital_admission_cum = cumsum(Hospital_admission),
    .after = Hospital_admission
  ) %>%
  mutate(
    Deceased_cum = cumsum(Deceased),
    .after = Deceased
  )
write.csv(rivm.mun.cum, file = "data-rivm/COVID-19_aantallen_gemeente_per_dag.csv", row.names = F)


## Data for municipalities

# Cumulative dataset 
rivm.municipalities <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv", sep=";")
last_date <- as.Date(last(rivm.municipalities$Date_of_report))
filename.municipality <- paste0("data-rivm/municipal-datasets/rivm_municipality_", last_date ,".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities, file=filename.municipality,row.names = F)


## Parse RIVM Daily data

temp = tail(list.files(path = "data-rivm/municipal-datasets-per-day/",pattern="*.csv", full.names = T),1)
dat <- read.csv(temp)

rivm.dailydata <- data.frame(as.Date(Sys.Date()),sum(dat$Total_reported),sum(dat$Hospital_admission),sum(dat$Deceased)) ## Calculate totals for cases, hospitalizations, deaths
names(rivm.dailydata) <- c("date","cases","hospitalization","deaths")

filename.daily <- paste0("data-rivm/data-per-day/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
write.csv(rivm.dailydata, file = filename.daily,row.names = F) ## Write file with daily data

temp = list.files(path = "data-rivm/data-per-day/",pattern="*.csv", full.names = T) ## Fetch all day files
myfiles = lapply(temp, read.csv) ## Load all day files

rivm_by_day <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

rivm_by_day$date <- as.Date(rivm_by_day$date)
rivm_by_day <- rivm_by_day[order(rivm_by_day$date),]

rivm_by_day <- rivm_by_day %>%
  mutate(positivetests = c(0,diff(cases))) # Calculate number of positive tests per day

rivm_by_day <- rivm_by_day %>%
  mutate(hospital_intake_rivm = c(0,diff(hospitalization))) %>%
  mutate(hospital_intake_rivm = replace(hospital_intake_rivm, hospital_intake_rivm<0, 0)) # Calculate number of hospitalizations per day

write.csv(rivm_by_day, file = "data/rivm_by_day.csv",row.names = F) ## Write file with aggregate data per day

rm(list=ls())