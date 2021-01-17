require(cowplot)
require(tidyverse)
require(rjson)
require(data.table)
require(jsonlite)

rm(list=ls())

# Data municipalities per day
rivm.mun.perday <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv", sep=";")

# Verify that new data has been uploaded
condition <- Sys.Date()!=as.Date(last(rivm.mun.perday$Date_of_report))

#if (condition) {stop("The value is TRUE, so the script must end here")    
#} else {

# Parse data municipality per day 
sum(rivm.mun.perday$Total_reported)-906956  
sum(rivm.mun.perday$Deceased)-12965
last_date <- as.Date(last(rivm.mun.perday$Date_of_report))
filename.mun.perday <- paste0("raw-data-archive/municipal-datasets-per-day/rivm_municipality_perday_", last_date, ".csv") ## Filename for daily data municipalities
write.csv(rivm.mun.perday, file=filename.mun.perday,row.names = F)

filename.mun.perday.compressed <- paste0("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_", last_date, ".csv.gz") ## Filename for daily data municipalities
write.csv(rivm.mun.perday, file=gzfile(filename.mun.perday.compressed),row.names = F)

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
write.csv(rivm.mun.cum, file = gzfile("data-rivm/COVID-19_aantallen_gemeente_per_dag.csv.gz"), row.names = F)

## Parse RIVM Daily data

temp = tail(list.files(path = "data-rivm/municipal-datasets-per-day/",pattern="*.csv.gz", full.names = T),1)
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

## Parse daily percentage positive tests - national ##

dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
tested_daily <- as.data.frame(dat$tested_ggd_daily[1])
tested_daily$date <- as.Date(as.POSIXct(tested_daily$values.date_unix, origin="1970-01-01"))
tested_daily$pos.rate.3d.avg <- round(frollmean(tested_daily[,"values.infected_percentage"],3),1)

write.csv(tested_daily, file = "data-dashboards/percentage-positive-daily-national.csv",row.names = F)

## Parse daily percentage positive tests - safety region ##

df.vr.dailytests <- data.frame()

for (i in 1:25) {
  if(i<10){
    db <- fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/json/VR0",i,".json"))
  }else{
    db <- fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/json/VR",i,".json"))
  }
  db <- as.data.frame(db$tested_ggd_daily[1])
  df.vr.dailytests <- rbind(df.vr.dailytests,db)
}
df.vr.dailytests$date <- as.Date(as.POSIXct(df.vr.dailytests$values.date_unix, origin="1970-01-01"))
df.vr.dailytests$pos.rate.3d.avg <- round(frollmean(df.vr.dailytests[,"values.infected_percentage"],3),1)

write.csv(df.vr.dailytests, file = "data-dashboards/percentage-positive-daily-safetyregion.csv",row.names = F)

#continue the script
print("Script did NOT end!")   
#}


rm(list=ls())
