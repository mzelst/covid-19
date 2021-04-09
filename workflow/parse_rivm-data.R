# Data municipalities per day
rivm.mun.perday <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv", sep=";")

# Verify that new data has been uploaded
#condition <- Sys.Date()!=as.Date(last(rivm.mun.perday$Date_of_report))

#if (condition) {stop("The value is TRUE, so the script must end here")    
#} else {

# Parse data municipality per day 
last_date <- as.Date(last(rivm.mun.perday$Date_of_report))
filename.mun.perday <- paste0("raw-data-archive/municipal-datasets-per-day/rivm_municipality_perday_", last_date, ".csv") ## Filename for daily data municipalities
fwrite(rivm.mun.perday, file=filename.mun.perday,row.names = F)

filename.mun.perday.compressed <- paste0("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_", last_date, ".csv.gz") ## Filename for daily data municipalities
fwrite(rivm.mun.perday, file=filename.mun.perday.compressed,row.names = F)

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
fwrite(rivm.mun.cum, file = "data-rivm/COVID-19_aantallen_gemeente_per_dag.csv.gz", row.names = F)

## Parse RIVM Daily data
rivm.dailydata <- data.frame(as.Date(Sys.Date()),sum(rivm.mun.cum$Total_reported),sum(rivm.mun.cum$Hospital_admission),sum(rivm.mun.cum$Deceased)) ## Calculate totals for cases, hospitalizations, deaths
names(rivm.dailydata) <- c("date","cases","hospitalization","deaths")

filename.daily <- paste0("data-rivm/data-per-day/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
fwrite(rivm.dailydata, file = filename.daily,row.names = F) ## Write file with daily data

temp = list.files(path = "data-rivm/data-per-day/",pattern="*.csv", full.names = T) ## Fetch all day files
myfiles = lapply(temp, fread) ## Load all day files

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

fwrite(rivm_by_day, file = "data/rivm_by_day.csv",row.names = F) ## Write file with aggregate data per day

## Download data disabled people 
disabled.people <- fread("https://data.rivm.nl/covid-19/COVID-19_gehandicaptenzorg.csv", sep = ";")
filename.disabledpeople.raw  <- paste0("raw-data-archive/disabled-people-per-day/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
fwrite(disabled.people, file = filename.disabledpeople.raw,row.names = F) 

filename.disabledpeople.compressed  <- paste0("data-rivm/disabled-people-per-day/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
fwrite(disabled.people, file = filename.disabledpeople.compressed,row.names = F) 

## Download data 70+ living at home 
living.home.70plus <- fread("https://data.rivm.nl/covid-19/COVID-19_thuiswonend_70plus.csv", sep = ";")
filename.living.home.70plus.raw <- paste0("raw-data-archive/70plus-living-at-home-per-day/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
fwrite(living.home.70plus, file = filename.living.home.70plus.raw,row.names = F) 

filename.living.home.70plus.compressed <- paste0("data-rivm/70plus-living-at-home-per-day/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
fwrite(living.home.70plus, file = filename.living.home.70plus.compressed,row.names = F) 

## Download behavior
behavior <- fread("https://data.rivm.nl/covid-19/COVID-19_gedrag.csv", sep = ";")
filename.behavior.raw <- paste0("raw-data-archive/behavior/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
fwrite(behavior, file = filename.behavior.raw,row.names = F) 

filename.behavior.compressed <- paste0("data-rivm/behavior/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
fwrite(behavior, file = filename.behavior.compressed,row.names = F) 

## Download nursing homes

nursing.homes <- fread("https://data.rivm.nl/covid-19/COVID-19_verpleeghuizen.csv", sep = ";")
filename.nursinghomes.raw <- paste0("raw-data-archive/nursing-home-datasets/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
fwrite(nursing.homes, file = filename.nursinghomes.raw,row.names = F)

filename.nursinghomes.compressed <- paste0("data-rivm/nursing-homes-datasets/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
fwrite(nursing.homes, file = filename.nursinghomes.compressed,row.names = F)

## Download tests

tests <- fread("https://data.rivm.nl/covid-19/COVID-19_uitgevoerde_testen.csv", sep = ";")
filename.tests.raw <- paste0("raw-data-archive/tests/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
fwrite(tests, file = filename.tests.raw,row.names = F)

filename.tests.compressed <- paste0("data-rivm/tests/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
fwrite(tests, file = filename.tests.compressed,row.names = F)

## Download IC data (NICE)

ic.nice.data <- fread("https://data.rivm.nl/covid-19/COVID-19_ic_opnames.csv", sep = ";")
filename.ic.nice <- paste0("data-rivm/ic-datasets/ic_daily_",last(ic.nice.data$Date_of_statistics),".csv") ## Filename for daily data
fwrite(ic.nice.data, file = filename.ic.nice,row.names = F)

#continue the script
print("Script did NOT end!")   
#}