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

write.csv(df.cases, file = "corrections/cases_perday.csv", row.names = F)

## Hospital
df.hospital <- df %>%
  dplyr::filter(Hospital_admission == "Yes")

df.hospitals <- dcast.data.table(df.hospital, Date_statistics + date ~ value, fun.aggregate = sum)

hospitals.wide <- spread(df.hospitals, key = date, value = `1`)
setDF(hospitals.wide)

# Calculate moving difference between cases per day
hospitals.wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- hospitals.wide[dates.lead] - hospitals.wide[dates.trail]

write.csv(hospitals.wide, file = "corrections/hospital_perday.csv", row.names = F)


## Deaths
df.death <- df %>%
  dplyr::filter(Deceased == "Yes")

df.deaths <- dcast.data.table(df.death, Date_statistics + date ~ value, fun.aggregate = sum)

deaths.wide <- spread(df.deaths, key = date, value = `1`)
setDF(deaths.wide)

# Calculate moving difference between cases per day
deaths.wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- deaths.wide[dates.lead] - deaths.wide[dates.trail]

write.csv(deaths.wide, file = "corrections/deaths_perday.csv", row.names = F)

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

# Git
git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Daily (automated) update date trackers ",Sys.Date()))
push(repo, credentials = git.auth)