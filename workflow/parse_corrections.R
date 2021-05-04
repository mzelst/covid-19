temp = tail(list.files(path = "data-rivm/municipal-datasets-per-day/",pattern="*.csv", full.names = T),2)
myfiles = lapply(temp, fread)

dat.today <- setDT(as.data.frame(myfiles[2]))
dat.yesterday <- setDT(as.data.frame(myfiles[1]))

# Positive tests
cases.today <- dat.today[, .(cases.today=sum(Total_reported)), by=Date_of_publication]
cases.yesterday <- dat.yesterday[, .(cases.yesterday=sum(Total_reported)), by=Date_of_publication]

df.cases.new <- merge(cases.today,cases.yesterday,by="Date_of_publication")
df.cases.new$diff <- df.cases.new$cases.today - df.cases.new$cases.yesterday

new.infection <- last(cases.today$cases.today)
corrections.cases <- sum(df.cases.new$diff)
net.infection <- new.infection+corrections.cases

## Hospitals
hospital.today <- dat.today[, .(hospital.today=sum(Hospital_admission)), by=Date_of_publication]
hospital.yesterday <- dat.yesterday[, .(hospital.yesterday=sum(Hospital_admission)), by=Date_of_publication]

df.hospital.new <- merge(hospital.today,hospital.yesterday,by="Date_of_publication")
df.hospital.new$diff <- df.hospital.new$hospital.today - df.hospital.new$hospital.yesterday

new.hospitals <- last(hospital.today$hospital.today)
corrections.hospitals <- sum(df.hospital.new$diff)
net.hospitals <- new.hospitals + corrections.hospitals

## Deaths
death.today <- dat.today[, .(death.today=sum(Deceased)), by=Date_of_publication]
death.yesterday <- dat.yesterday[, .(death.yesterday=sum(Deceased)), by=Date_of_publication]

df.death.new <- merge(death.today,death.yesterday,by="Date_of_publication")
df.death.new$diff <- df.death.new$death.today - df.death.new$death.yesterday

new.deaths <- last(death.today$death.today)
corrections.deaths <- sum(df.death.new$diff)
net.deaths <- new.deaths+corrections.deaths


corrections.all <- as.data.frame(cbind(new.infection,corrections.cases, net.infection,new.hospitals,corrections.hospitals, 
                                       net.hospitals,new.deaths,corrections.deaths,net.deaths))

corrections.all$date <- as.Date(Sys.Date())

filename <- paste0("corrections/corrections_per_day/corrections-",Sys.Date(),'.csv')
write.csv(corrections.all, file = filename, row.names=F)

temp = list.files(path = "corrections/corrections_per_day/",pattern="*.csv", full.names = T)
myfiles = lapply(temp, read.csv)
corrections.perday <- dplyr::bind_rows(myfiles)
corrections.perday$positive_7daverage <- round(frollmean(corrections.perday[,"new.infection"],7),0) # Calculate 7-day average (based on newly reported infections, gross number)

write.csv(corrections.perday, file = "corrections/corrections_perday.csv", row.names = FALSE)

rm(list=ls())
