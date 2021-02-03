require(tidyverse)
require(data.table)
require(rjson)
require(lubridate)
rm(list=ls())
#### Corrections scripts

temp = tail(list.files(path = "data-rivm/casus-datasets/",pattern="*.csv.gz", full.names = T),2)
myfiles = lapply(temp, fread)

df <- map_dfr(myfiles, ~{
  .x
})

df$value <- 1
df$date <- as.Date(parse_date_time(df$Date_file, "Ymd HMS"))

df.cases <- dcast.data.table(df, Date_statistics + date ~ value, fun.aggregate = sum)
df.cases <- spread(df.cases, key = date, value = `1`)

col.start.diff <- ncol(df.cases)+1

dates.lead <- names(df.cases)[3:ncol(df.cases)] ## Set lead colnames for diff
dates.trail <- names(df.cases)[2:(ncol(df.cases)-1)] ## Set trail colnames for diff

setDF(df.cases)

# Calculate moving difference between cases per day
df.cases[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- df.cases[dates.lead] - df.cases[dates.trail]

write.csv(df.cases, file = "corrections/cases_perday.csv")

## Hospital
df.hospital <- df %>%
  dplyr::filter(Hospital_admission == "Yes")

df.hospitals <- dcast.data.table(df.hospital, Date_statistics + date ~ value, fun.aggregate = sum)

hospitals.wide <- spread(df.hospitals, key = date, value = `1`)
setDF(hospitals.wide)

# Calculate moving difference between cases per day
hospitals.wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- hospitals.wide[dates.lead] - hospitals.wide[dates.trail]

write.csv(hospitals.wide, file = "corrections/hospital_perday.csv")


## Deaths
df.death <- df %>%
  dplyr::filter(Deceased == "Yes")

df.deaths <- dcast.data.table(df.death, Date_statistics + date ~ value, fun.aggregate = sum)

deaths.wide <- spread(df.deaths, key = date, value = `1`)
setDF(deaths.wide)

# Calculate moving difference between cases per day
deaths.wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- deaths.wide[dates.lead] - deaths.wide[dates.trail]

write.csv(deaths.wide, file = "corrections/deaths_perday.csv")

## Week of death - diff file
dat.today <- as.data.frame(myfiles[2])
dat.yesterday <- as.data.frame(myfiles[1])

dat.today$Week <- substr(dat.today$Week_of_death, 5, 6)
dat.yesterday$Week <- substr(dat.yesterday$Week_of_death, 5, 6)

dat.today$Year <- substr(dat.today$Week_of_death, 1, 4)
dat.yesterday$Year <- substr(dat.yesterday$Week_of_death, 1, 4)

today.weekdeath <- count(dat.today, Week,Year)
yesterday.weekdeath <- count(dat.yesterday, Week,Year)

df.weekdeath <- merge(today.weekdeath,yesterday.weekdeath,by=c("Week","Year"),all.X=T)
df.weekdeath$diff <- df.weekdeath$n.x - df.weekdeath$n.y
colnames(df.weekdeath) <- c("Week","Year","weekdeath_today","weekdeath_yesterday","diff")
df.weekdeath <- df.weekdeath[1:(nrow(df.weekdeath)-1),]
df.weekdeath <- df.weekdeath[order(df.weekdeath$Year),]
write.csv(df.weekdeath, file = "corrections/deaths_perweek.csv", row.names = F)

## Date of death - per GGD - diff file

dat.today <- setDT(as.data.frame(myfiles[2]))
dat.yesterday <- setDT(as.data.frame(myfiles[1]))

dat.today$death.today <- ifelse(dat.today$Deceased=="Yes",1,0)
dat.yesterday$death.yesterday <- ifelse(dat.yesterday$Deceased=="Yes",1,0)

death.today <- dat.today[, .(death.today=sum(death.today)), by=list(Date_statistics, Municipal_health_service)]
death.yesterday <- dat.yesterday[, .(death.yesterday=sum(death.yesterday)), by=list(Date_statistics, Municipal_health_service)]

df.death.new <- merge(death.today,death.yesterday,by=c("Date_statistics","Municipal_health_service"))
df.death.new$diff <- df.death.new$death.today-df.death.new$death.yesterday

df.death.new.corr <- df.death.new %>%
  filter(diff > 0 | diff < 0)

write.csv(df.death.new.corr, file = "corrections/deaths_perggd.csv", row.names = F)

## Date of cases - per GGD - diff file

dat.today$cases.today <- 1
dat.yesterday$cases.yesterday <- 1

cases.today <- dat.today[, .(cases.today=sum(cases.today)), by=list(Date_statistics, Municipal_health_service)]
cases.yesterday <- dat.yesterday[, .(cases.yesterday=sum(cases.yesterday)), by=list(Date_statistics, Municipal_health_service)]

df.cases.new <- merge(cases.today,cases.yesterday,by=c("Date_statistics","Municipal_health_service"))
df.cases.new$diff <- df.cases.new$cases.today-df.cases.new$cases.yesterday

df.cases.new.corr <- df.cases.new %>%
  filter(diff > 0 | diff < 0)

write.csv(df.cases.new.corr, file = "corrections/cases_perggd.csv", row.names = F)

## Date of cases - per municipality - diff file

temp = tail(list.files(path = "data-rivm/municipal-datasets-per-day/",pattern="*.csv.gz", full.names = T),2)
myfiles = lapply(temp, fread)

dat.today.mun <- setDT(as.data.frame(myfiles[2]))
dat.yesterday.mun <- setDT(as.data.frame(myfiles[1]))

cases.today.mun <- dat.today.mun[, .(cases.today.mun=sum(Total_reported)), by=list(Date_of_publication, Municipality_name,Municipal_health_service)]
cases.yesterday.mun <- dat.yesterday.mun[, .(cases.yesterday.mun=sum(Total_reported)), by=list(Date_of_publication, Municipality_name,Municipal_health_service)]

df.cases.new.mun <- merge(cases.today.mun,cases.yesterday.mun,by=c("Date_of_publication","Municipality_name", "Municipal_health_service"))
df.cases.new.mun$diff <- df.cases.new.mun$cases.today.mun-df.cases.new.mun$cases.yesterday.mun

df.cases.new.corr.mun <- df.cases.new.mun %>%
  filter(diff > 0 | diff < 0)

write.csv(df.cases.new.corr.mun, file = "corrections/cases_per_municipality.csv", row.names = F)

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

# Git
git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Daily (automated) update date trackers ",Sys.Date()))
push(repo, credentials = git.auth)