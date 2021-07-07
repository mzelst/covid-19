#### NICE CHECKER ####

source("workflow/parse_nice-data.R")

temp = tail(list.files(path = "data-nice/data-nice-json/",pattern="*.csv", full.names = T),4)
myfiles = lapply(temp, read.csv)

dat.today <- as.data.frame(myfiles[4])
dat.yesterday <- as.data.frame(myfiles[3])
dat.twodaysago <- as.data.frame(myfiles[2])
dat.threedaysago <- as.data.frame(myfiles[1])

sum(dat.today$Hospital_Intake_Proven) - sum(dat.yesterday$Hospital_Intake_Proven)
last(dat.today$Hospital_Currently)
sum(dat.today$IC_Intake_Proven) - sum(dat.yesterday$IC_Intake_Proven)
last(dat.today$IC_Current)

dat.today <- dat.today %>%
  mutate(Hospital_Intake_7d = round(frollmean(Hospital_Intake_Proven,7),0)) %>%
  mutate(IC_Intake_7d = round(frollmean(IC_Intake_Proven,7),0))

df <- merge(dat.today[,c("date","Hospital_Intake_Proven","Hospital_Intake_Suspected","IC_Intake_Proven","Hospital_Currently","IC_Current")], dat.yesterday[,c("date","Hospital_Intake_Proven","Hospital_Intake_Suspected","IC_Intake_Proven","Hospital_Currently","IC_Current")], by = "date", all.x=T)
df$diff.proven <- df$Hospital_Intake_Proven.x-df$Hospital_Intake_Proven.y
df$diff.suspec <- df$Hospital_Intake_Suspected.x-df$Hospital_Intake_Suspected.y
df$diff.proven.ic <- df$IC_Intake_Proven.x-df$IC_Intake_Proven.y
df$diff.current.hosp <- df$Hospital_Currently.x-df$Hospital_Currently.y
df$diff.current.ic <- df$IC_Current.x-df$IC_Current.y

#rm(myfiles,temp,dat.threedaysago,dat.twodaysago,dat.yesterday,dat.today,df,vaccine.data,dat)
