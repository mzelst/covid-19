require(tidyverse)
require(data.table)
rm(list=ls())
#### Corrections scripts

temp = tail(list.files(path = "data-rivm/casus-datasets/",pattern="*.csv", full.names = T),2)
myfiles = lapply(temp, read.csv)

df <- map_dfr(myfiles, ~{
  .x
})

df$date <- as.Date(df$Date_file)
df.cases <- as.data.frame(table(df$Date_statistics,df$date)) ## Success

df.cases <- spread(df.cases, key = Var2, value = Freq)

col.start.diff <- ncol(df.cases)+1

dates.lead <- names(df.cases)[3:ncol(df.cases)] ## Set lead colnames for diff
dates.trail <- names(df.cases)[2:(ncol(df.cases)-1)] ## Set trail colnames for diff

# Calculate moving difference between cases per day
df.cases[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- df.cases[dates.lead] - df.cases[dates.trail]

write.csv(df.cases, file = "corrections/cases_perday.csv")


## Hospital
df.hospital <- df %>%
  dplyr::filter(Hospital_admission == "Yes")

df.hospitals <- as.data.frame(table(df.hospital$Date_statistics,df.hospital$date)) ## Success

hospitals.wide <- spread(df.hospitals, key = Var2, value = Freq)

# Calculate moving difference between cases per day
hospitals.wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- hospitals.wide[dates.lead] - hospitals.wide[dates.trail]

write.csv(hospitals.wide, file = "corrections/hospital_perday.csv")


## Deaths
df.death <- df %>%
  dplyr::filter(Deceased == "Yes")

df.deaths <- as.data.frame(table(df.death$Date_statistics,df.death$date)) ## Success

deaths.wide <- spread(df.deaths, key = Var2, value = Freq)

# Calculate moving difference between cases per day
deaths.wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- deaths.wide[dates.lead] - deaths.wide[dates.trail]

write.csv(deaths.wide, file = "corrections/deaths_perday.csv")



## Week of death - diff file

temp = tail(list.files(path = "data-rivm/casus-datasets/",pattern="*.csv", full.names = T),2)
myfiles = lapply(temp, read.csv)

dat.today <- as.data.frame(myfiles[2])
dat.yesterday <- as.data.frame(myfiles[1])

dat.today$Week <- substr(dat.today$Week_of_death, 5, 6)
dat.yesterday$Week <- substr(dat.yesterday$Week_of_death, 5, 6)
today.weekdeath <- count(dat.today,Week)
yesterday.weekdeath <- count(dat.yesterday,Week)

df.weekdeath <- merge(today.weekdeath,yesterday.weekdeath,by="Week",all.X=T)
df.weekdeath$diff <- df.weekdeath$n.x - df.weekdeath$n.y
colnames(df.weekdeath) <- c("Week","weekdeath_today","weekdeath_yesterday","diff")
df.weekdeath <- df.weekdeath[1:(nrow(df.weekdeath)-1),]

df.weekdeath$year <- 2020
write.csv(df.weekdeath, file = "corrections/deaths_perweek.csv", row.names = F)

## Date of death - per GGD - diff file

dat.today$death.today <- ifelse(dat.today$Deceased=="Yes",1,0)
dat.yesterday$death.yesterday <- ifelse(dat.yesterday$Deceased=="Yes",1,0)

death.today <- aggregate(death.today ~ Date_statistics + Municipal_health_service, data = dat.today, FUN = sum)
death.yesterday <- aggregate(death.yesterday ~ Date_statistics + Municipal_health_service, data = dat.yesterday, FUN = sum)

df.death.new <- merge(death.today,death.yesterday,by=c("Date_statistics","Municipal_health_service"))
df.death.new$diff <- df.death.new$death.today-df.death.new$death.yesterday

df.death.new.corr <- df.death.new %>%
  filter(diff > 0 | diff < 0)

write.csv(df.death.new.corr, file = "corrections/deaths_perggd.csv", row.names = F)

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Daily (automated) update date trackers ",Sys.Date()))
push(repo, credentials = git.auth)


rm(list=ls())
