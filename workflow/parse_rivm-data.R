rm(list=ls())

rivm.data <- read.csv("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv", sep=";") ## Read in data with all cases until today
filename <- paste0("data-rivm/casus-datasets/COVID-19_casus_landelijk_",Sys.Date(),".csv")

paste0("data-rivm/casus-data/COVID-19_casus_landelijk_",Sys.Date(),".csv")
write.csv(rivm.data, file=filename,row.names = F) ## Write file with all cases until today

rivm.dailydata <- data.frame(as.Date(Sys.Date()),nrow(rivm.data),sum(rivm.data$Hospital_admission == "Yes"),sum(rivm.data$Deceased == "Yes")) ## Calculate totals for cases, hospitalizations, deaths
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

## Data for municipalities

# Cumulative dataset 
rivm.municipalities <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv", sep=";")
filename.municipality <- paste0("data-rivm/municipal-datasets/rivm_municipality_",Sys.Date(),".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities, file=filename.municipality,row.names = F)

# Data per day

rivm.municipalities.perday <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv",sep=";")
filename.municipality.perday <- paste0("data-rivm/municipal-datasets-per-day/rivm_municipality_",Sys.Date(),".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities.perday, file=filename.municipality.perday,row.names = F)

rm(list=ls())