## Stichting NICE data

# IC patients died, discharged, discharged to other department (cumulative)
ics.used <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/ic-count",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

ic.died_survivors <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/died-and-survivors-cumulative", simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

ic.death_survive <- as.data.frame(t(ic.died_survivors[c(2,4,6),]))

ic.death_survive$ic_deaths <- unlist(ic.death_survive$V1)
ic.death_survive$ic_discharge <- unlist(ic.death_survive$V2)
ic.death_survive$ic_discharge_inhosp <- unlist(ic.death_survive$V3)
ic.death_survive <- ic.death_survive[,c(4:6)]

# New patients at IC 
ic_intake <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

ic_intake <- as.data.frame(t(ic_intake[c(2,4),]))

ic_intake$ic_intake_proven <- unlist(ic_intake$V1)
ic_intake$ic_intake_suspected <- unlist(ic_intake$V2)
ic_intake <- ic_intake[,c(3:4)]

# IC patients currently
ic_current <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# IC patients cumulative
ic.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Number of patients currently in hospital (non-IC) 
zkh_current <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Intake per day of patients in hospital (non-IC) with suspected and/or proven covid-19
json_zkh_df <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

zkh_new <- as.data.frame(t(json_zkh_df[c(1,2,4),]))

zkh_new$date <- unlist(zkh_new$V1)
zkh_new$new_hosp_proven <- unlist(zkh_new$V2)
zkh_new$new_hosp_suspected <- unlist(zkh_new$V3)
zkh_new <- zkh_new[,c(4:6)]

hospital.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-cumulative/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Merge all data
df <- data.frame(zkh_new,ic_intake,ic_current$value,ics.used$value,ic.cumulative$value,zkh_current$value,ic.death_survive, hospital.cumulative$value)
names(df) <- c("date","Hospital_Intake_Proven","Hospital_Intake_Suspected","IC_Intake_Proven","IC_Intake_Suspected","IC_Current","ICs_Used","IC_Cumulative","Hospital_Currently","IC_Deaths_Cumulative","IC_Discharge_Cumulative","IC_Discharge_InHospital","Hospital_Cumulative")
df$Hospital_Intake <- df$Hospital_Intake_Proven + df$Hospital_Intake_Suspected
df$IC_Intake <- df$IC_Intake_Proven + df$IC_Intake_Suspected

# Cumulative sums for suspected cases in hospital
df <- df %>% mutate(Hosp_Intake_Suspec_Cumul = cumsum(Hospital_Intake_Suspected))
df <- df %>% mutate(IC_Intake_Suspected_Cumul = cumsum(IC_Intake_Suspected))
df$date <- as.Date(df$date)

df$IC_Intake_Proven_Cumsum <- cumsum(df$IC_Intake_Proven)

write.csv(df, "data-nice/nice-today.csv", row.names = F) ## Write file with all NICE data until today
filename.nice.perday <- paste0("data-nice/data-nice-json/",Sys.Date(),".csv")
write.csv(df, filename.nice.perday, row.names = F) ## Save daily NICE data - JSON parsed - downloaded around 14:30 PM (CET)

## Daily NICE data
nice.dailydata <- last(df)
filename.daily.nice <- paste0("data-nice/data-per-day/nice_daily_",Sys.Date(),".csv") ## Filename for daily data
write.csv(nice.dailydata, file = filename.daily.nice, row.names = F) ## Write file with daily data

temp = list.files(path = "data-nice/data-per-day/",pattern="*.csv", full.names = T) ## Fetch all day files
myfiles = lapply(temp, read.csv) ## Load all day files

nice_by_day <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

nice_by_day$date <- as.Date(nice_by_day$date)
nice_by_day <- nice_by_day[order(nice_by_day$date),]

nice_by_day <- nice_by_day %>%
  mutate(ic_intake_nice = c(0,diff(IC_Cumulative))) %>%
  mutate(ic_intake_nice = replace(ic_intake_nice, ic_intake_nice<0, 0))# Calculate number of positive tests per day
           
write.csv(nice_by_day, file = "data/nice_by_day.csv", row.names = F)

## Leeftijd op IC

leeftijd.ic <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/age-distribution-status/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

leeftijd.ic <- as.data.frame(t(leeftijd.ic[c(1,2,4,6,8),]))

leeftijd.ic$V1 <- unlist(leeftijd.ic$V1)
leeftijd.ic$V2 <- unlist(leeftijd.ic$V2)
leeftijd.ic$V3 <- unlist(leeftijd.ic$V3)
leeftijd.ic$V4 <- unlist(leeftijd.ic$V4)
leeftijd.ic$V5 <- unlist(leeftijd.ic$V5)

colnames(leeftijd.ic) <- c("Leeftijd","IC_naar_Klinisch","IC_aanwezig","Verlaten_Levend","Verlaten_Overleden")

leeftijd.ic$Totaal <- rowSums(leeftijd.ic[,c(2:5)])
leeftijd.ic$Datum <- as.Date(Sys.Date())

filename.IC <- paste0("data-nice/age/IC/nice_daily_age_IC_",Sys.Date(),".csv")

write.csv(leeftijd.ic, file = filename.IC, row.names = F)

## Leeftijd op Klinische afdeling

leeftijd.klinisch <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/age-distribution-status/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

leeftijd.klinisch <- as.data.frame(t(leeftijd.klinisch[c(1,2,4,6),]))

leeftijd.klinisch$V1 <- unlist(leeftijd.klinisch$V1)
leeftijd.klinisch$V2 <- unlist(leeftijd.klinisch$V2)
leeftijd.klinisch$V3 <- unlist(leeftijd.klinisch$V3)
leeftijd.klinisch$V4 <- unlist(leeftijd.klinisch$V4)


colnames(leeftijd.klinisch) <- c("Leeftijd","Klinisch_aanwezig","Verlaten_Levend","Verlaten_Overleden")

leeftijd.klinisch$Totaal <- rowSums(leeftijd.klinisch[,c(2:4)])
leeftijd.klinisch$Datum <- as.Date(Sys.Date())

filename.klinisch <- paste0("data-nice/age/Clinical_Beds/nice_daily_age_clinical_",Sys.Date(),".csv")

write.csv(leeftijd.klinisch, file = filename.klinisch, row.names = F)

## Exit IC

ic.died.left <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/died-and-survivors-cumulative/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

ic.died.left <- as.data.frame(t(ic.died.left[c(1,2,4,6),]))
ic.died.left$V1 <- unlist(ic.died.left$V1)
ic.died.left$V2 <- unlist(ic.died.left$V2)
ic.died.left$V3 <- unlist(ic.died.left$V3)
ic.died.left$V4 <- unlist(ic.died.left$V4)

colnames(ic.died.left) <- c("date","Overleden","Ontslagen","IC_to_Clinical_Alive")
filename.IC.exit <- paste0("data-nice/exit/IC/nice_daily_exit_IC_",Sys.Date(),".csv")
write.csv(ic.died.left, file = filename.IC.exit, row.names = F)

## Exit Clinical beds

zkh.died.left <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/died-and-survivors-cumulative/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

zkh.died.left <- as.data.frame(t(zkh.died.left[c(1,2,4),]))
zkh.died.left$V1 <- unlist(zkh.died.left$V1)
zkh.died.left$V2 <- unlist(zkh.died.left$V2)
zkh.died.left$V3 <- unlist(zkh.died.left$V3)

colnames(zkh.died.left) <- c("date","Overleden","Ontslagen")

zkh.died.left <- zkh.died.left %>%
  mutate(Overleden_pdag = c(0,diff(Overleden))) %>%
  mutate(Ontslagen_pdag = c(0,diff(Ontslagen)))

filename.klinisch.exit <- paste0("data-nice/exit/Clinical_Beds/nice_daily_exit_clinical_",Sys.Date(),".csv")

write.csv(zkh.died.left, file = filename.klinisch.exit, row.names = F)

#### Treatment Time IC ####

ligduur.ic <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/behandelduur-distribution/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)
ligduur.ic <- as.data.frame(t(ligduur.ic[c(1,2,4,6,8),]))
colnames(ligduur.ic) <- c("dagen","IC_to_clinical","Treatment_time_hospitalized","Treatment_time_to_exit","Treatment_time_to_death")
filename.ligduur.ic <- paste0("data-nice/treatment-time/IC/nice_daily_treatment-time_IC_",Sys.Date(),".csv")
write.csv(ligduur.ic, file = filename.ligduur.ic, row.names = F)

#### Treatment Time clinical beds ####

ligduur.clinical <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/behandelduur-distribution/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)
ligduur.clinical <- as.data.frame(t(ligduur.clinical[c(1,2,4,6),]))
colnames(ligduur.clinical) <- c("dagen","Treatment_time_hospitalized","Treatment_time_to_exit","Treatment_time_to_death")
filename.ligduur.clinical <- paste0("data-nice/treatment-time/Clinical_Beds/nice_daily_treatment-time_clinical_",Sys.Date(),".csv")
write.csv(ligduur.clinical, file = filename.ligduur.clinical, row.names = F)

rm("df","filename.daily.nice","filename.nice.perday","hospital.cumulative","ic.cumulative","ic.death_survive",
   "ic.died_survivors","ic_current","ic_intake","ics.used","json_zkh_df","nice.dailydata","zkh_current","zkh_new","temp","myfiles",
   "nice_by_day", "ic.died.left","leeftijd.ic","leeftijd.klinisch","ligduur.clinical","ligduur.ic","zkh.died.left",
   "filename.IC","filename.IC.exit","filename.klinisch","filename.klinisch.exit","filename.ligduur.ic",
   "filename.ligduur.clinical")
