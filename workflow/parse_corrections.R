require(tidyverse)
require(data.table)
rm(list=ls())
#### Corrections scripts

temp = list.files(path = "data-rivm/casus-datasets/",pattern="*.csv", full.names = T)
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

neg.values <- c()
for(i in col.start.diff:ncol(df.cases)) {
  neg.values[i] <- sum(df.cases[,i][df.cases[,i]<0], na.rm=TRUE)
  neg.values <- c(neg.values)
}

neg.counts <- neg.values[col.start.diff:ncol(df.cases)]
corrections.cases <- neg.values[col.start.diff:ncol(df.cases)]

cases.perday <- apply(df.cases[,c(2:(col.start.diff-1))],2,sum,na.rm = T)
net.diff <- cases.perday - lag(cases.perday,1)
net.diff <- net.diff[2:length(net.diff)]

new.infection <- neg.counts*-1+net.diff ## Calculate new cases
net.infection <- new.infection + corrections.cases

## Hospital
df.hospital <- df %>%
  dplyr::filter(Hospital_admission == "Yes")

df.hospitals <- as.data.frame(table(df.hospital$Date_statistics,df.hospital$date)) ## Success

hospitals.wide <- spread(df.hospitals, key = Var2, value = Freq)

# Calculate moving difference between cases per day
hospitals.wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- hospitals.wide[dates.lead] - hospitals.wide[dates.trail]

write.csv(hospitals.wide, file = "corrections/hospital_perday.csv")

neg.values <- c()
for(i in col.start.diff:ncol(hospitals.wide)) {
  neg.values[i] <- sum(hospitals.wide[,i][hospitals.wide[,i]<0], na.rm=TRUE)
  neg.values <- c(neg.values)
}

neg.counts <- neg.values[col.start.diff:ncol(hospitals.wide)]
corrections.hospitals <- neg.values[col.start.diff:ncol(hospitals.wide)]

cases.perday <- apply(hospitals.wide[,c(2:(col.start.diff-1))],2,sum,na.rm = T)
net.diff <- cases.perday - lag(cases.perday,1)
net.diff <- net.diff[2:length(net.diff)]

new.hospitals <- neg.counts*-1+net.diff ## Calculate new cases
net.hospitals <- new.hospitals + corrections.hospitals

## Deaths
df.death <- df %>%
  dplyr::filter(Deceased == "Yes")

df.deaths <- as.data.frame(table(df.death$Date_statistics,df.death$date)) ## Success

deaths.wide <- spread(df.deaths, key = Var2, value = Freq)

# Calculate moving difference between cases per day
deaths.wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- deaths.wide[dates.lead] - deaths.wide[dates.trail]

write.csv(deaths.wide, file = "corrections/deaths_perday.csv")

neg.values <- c()
for(i in col.start.diff:ncol(deaths.wide)) {
  neg.values[i] <- sum(deaths.wide[,i][deaths.wide[,i]<0], na.rm=TRUE)
  neg.values <- c(neg.values)
}

neg.counts <- neg.values[col.start.diff:ncol(deaths.wide)]
corrections.deaths <- neg.values[col.start.diff:ncol(deaths.wide)]

cases.perday <- apply(deaths.wide[,c(2:(col.start.diff-1))],2,sum,na.rm = T)
net.diff <- cases.perday - lag(cases.perday,1)
net.diff <- net.diff[2:length(net.diff)]

new.deaths <- neg.counts*-1+net.diff ## Calculate new cases
net.deaths <- new.deaths + corrections.deaths

## Merge all correction data
corrections.all <- as.data.frame(cbind(new.infection,corrections.cases, net.infection,new.hospitals,corrections.hospitals, 
                                       net.hospitals,new.deaths,corrections.deaths,net.deaths))

corrections.all$date <- as.Date(rownames(corrections.all))


write.csv(corrections.all, file = "corrections/corrections_perday.csv")
rm(list=ls())

corrections.perday <- read.csv("corrections/corrections_perday.csv")


