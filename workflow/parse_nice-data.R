require(tidyverse)
require(rjson)
require(data.table)
rm(list=ls())
## Stichting NICE data

# IC patients died, discharged, discharged to other department (cumulative)
ics.used <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/ic-count",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

ic.died_survivors <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/died-and-survivors-cumulative", simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

ic.death_survive <- as.data.frame(t(ic.died_survivors[c(2,4,6),]))

ic.death_survive$ic_deaths <- unlist(ic.death_survive$V1)
ic.death_survive$ic_discharge <- unlist(ic.death_survive$V2)
ic.death_survive$ic_discharge_inhosp <- unlist(ic.death_survive$V3)
ic.death_survive <- ic.death_survive[,c(4:6)]

# New patients at IC 
ic_intake <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

ic_intake <- as.data.frame(t(ic_intake[c(2,4),]))

ic_intake$ic_intake_proven <- unlist(ic_intake$V1)
ic_intake$ic_intake_suspected <- unlist(ic_intake$V2)
ic_intake <- ic_intake[,c(3:4)]

# IC patients currently
ic_current <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# IC patients cumulative
ic.cumulative <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Number of patients currently in hospital (non-IC) 
zkh_current <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Intake per day of patients in hospital (non-IC) with suspected and/or proven covid-19
json_zkh_df <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

zkh_new <- as.data.frame(t(json_zkh_df[c(1,2,4),]))

zkh_new$date <- unlist(zkh_new$V1)
zkh_new$new_hosp_proven <- unlist(zkh_new$V2)
zkh_new$new_hosp_suspected <- unlist(zkh_new$V3)
zkh_new <- zkh_new[,c(4:6)]


# Merge all data
df <- data.frame(zkh_new,ic_intake,ic_current$value,ics.used$value,ic.cumulative$value,zkh_current$value,ic.death_survive)
names(df) <- c("date","Hospital_Intake_Proven","Hospital_Intake_Suspected","IC_Intake_Proven","IC_Intake_Suspected","IC_Current","ICs_Used","IC_Cumulative","Hospital_Currently","IC_Deaths_Cumulative","IC_Discharge_Cumulative","IC_Discharge_InHospital")
df$Hospital_Intake <- df$Hospital_Intake_Proven + df$Hospital_Intake_Suspected
df$IC_Intake <- df$IC_Intake_Proven + df$IC_Intake_Suspected

# Cumulative sums for suspected cases in hospital
df <- df %>% mutate(Hosp_Intake_Suspec_Cumul = cumsum(Hospital_Intake_Suspected))
df <- df %>% mutate(IC_Intake_Suspected_Cumul = cumsum(IC_Intake_Suspected))
df$date <- as.Date(df$date)

write.csv(df, "data-nice/nice-today.csv") ## Write file with all NICE data until today
filename.nice.perday <- paste0("data-nice/data-nice-json/",Sys.Date(),".csv")
write.csv(df, filename.nice.perday) ## Save daily NICE data - JSON parsed - downloaded around 14:30 PM (CET)

## Daily NICE data
nice.dailydata <- last(df)
filename.daily.nice <- paste0("data-nice/data-per-day/nice_daily_",Sys.Date(),".csv") ## Filename for daily data
write.csv(nice.dailydata, file = filename.daily.nice) ## Write file with daily data

nice.daily_aggregate <- read.csv("data/nice_by_day.csv") ## Read in aggregate data
nice.daily_aggregate <- nice.daily_aggregate[,-1] ## Remove identifier column

nice.daily_aggregate <- rbind(nice.dailydata, nice.daily_aggregate) ## Bind data today with aggregate data per day
nice.daily_aggregate <- nice.daily_aggregate[order(nice.daily_aggregate$date),]
nice.daily_aggregate$date <- as.Date(nice.daily_aggregate$date)

write.csv(nice.daily_aggregate, file = "data/nice_by_day.csv")

rm(list=ls())
