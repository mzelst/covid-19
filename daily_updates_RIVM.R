require(rvest)
require(RSelenium)
require(tidyverse)
require(rjson)
require(rtweet)
require(data.table)

get_token()

setwd("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19")

rivm.data <- read.csv("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv", sep=";") ## Read in data with all cases until today
filename <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_total_cumulative/COVID-19_casus_landelijk_",Sys.Date(),".csv")

write.csv(rivm.data, file=filename) ## Write file with all cases until today

rivm.data$Week <- substr(rivm.data$Week_of_death, 5, 6) ## Add week of death

rivm.death <- rivm.data %>%
  dplyr::filter(Deceased == "Yes") ## Extract deaths data only

rivm.hospital <- rivm.data %>%
  dplyr::filter(Hospital_admission == "Yes") ## Extract hospital data only

rivm.dailydata <- data.frame(Sys.Date(),nrow(rivm.data),nrow(rivm.hospital),nrow(rivm.death)) ## Calculate totals for cases, hospitalizations, deaths
names(rivm.dailydata) <- c("date","cases","hospitalization","deaths")

filename.daily <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_data/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data

write.csv(rivm.dailydata, file = filename.daily) ## Write file with daily data

rivm.daily_aggregate <- read.csv("rivm.daily_aggregate.csv") ## Read in aggregate data
rivm.daily_aggregate <- rivm.daily_aggregate[,-1] ## Remove identifier column

rivm.daily_aggregate <- rbind(rivm.dailydata, rivm.daily_aggregate) ## Bind data today with aggregate data per day
write.csv(rivm.daily_aggregate, file = "C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/rivm.daily_aggregate.csv") ## Write file with aggregate data per day

## Data for municipalities

rivm.municipalities <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv", sep=";")
filename.municipality <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_municipality_cumulative/rivm_municipality_",Sys.Date(),".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities, file=filename.municipality)

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

df <- df %>% mutate(Hosp_Intake_Suspec_Cumul = cumsum(Hospital_Intake_Suspected))
df <- df %>% mutate(IC_Intake_Suspected_Cumul = cumsum(IC_Intake_Suspected))

write.csv(df, "C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_nice_data/Cumulative_NICE.csv") ## Write file with all cases until today

df_diff <- as.data.frame(sapply(df[,c(2:12)], function(x) tail(diff(x),n=1)))
df_diff['IC_Current',]

## Build tweets

cases.yesterday <- head(diff(rivm.daily_aggregate$cases),n=1)*-1 ## Calculate new cases
hospital.yesterday <- head(diff(rivm.daily_aggregate$hospitalization),n=1)*-1 ## Calculate new hospitalizations
deaths.yesterday <- head(diff(rivm.daily_aggregate$deaths),n=1)*-1 ## Calculate new deaths
ic.yesterday <- tail(diff(ic.cumulative$value),n=1) ## Calculate new IC intakes

cases.patient <- ifelse(cases.yesterday == 1, "patiënt","patiënten")
hospital.patient <- ifelse(hospital.yesterday == 1, "patiënt","patiënten")
deaths.patient <- ifelse(deaths.yesterday == 1, "patiënt","patiënten")
ic.patient <- ifelse(ic.yesterday == 1, "patiënt","patiënten")


## Build tweets
tweet <- paste0("Dagelijkse corona-cijfers update: 

",cases.yesterday," ",cases.patient," positief getest 
(totaal: ",nrow(rivm.data),") 
",
hospital.yesterday," ",hospital.patient," opgenomen 
(totaal: ",nrow(rivm.hospital),") 
",
ic.yesterday," ",ic.patient," opgenomen op de IC 
(totaal: ",tail(ic.cumulative$value,n=1),") 
",
deaths.yesterday," ",deaths.patient," overleden 
(totaal: ",nrow(rivm.death),")")

tweet

post_tweet(status = tweet) ## Post tweet

my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
reply_id <- my_timeline$status_id[1] ## Status ID for reply
post_tweet("Voor een veel uitgebreidere update verwijs ik graag naar de dagelijkse updates van @edwinveldhuizen die dit ook per gemeente doet. Wij kijken samen nu ook of we de correcties beter in beeld kunnen krijgen.",
           in_reply_to_status_id = reply_id) ## Post reply

post_tweet(status = "Het RIVM publiceert nu de wekelijkse updates op dinsdag (vandaag dus). Zie voor de update over afgelopen week de site van het @RIVM: https://www.rivm.nl/coronavirus-covid-19/actueel",in_reply_to_status_id = reply_id)
