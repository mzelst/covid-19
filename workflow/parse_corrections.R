
temp = tail(list.files(path = "data-rivm/municipal-datasets-per-day/",pattern="*.csv", full.names = T),2)
myfiles = lapply(temp, read.csv)

dat.today <- as.data.frame(myfiles[2])
dat.yesterday <- as.data.frame(myfiles[1])

# Positive tests
net.infection <- sum(dat.today$Total_reported) - sum(dat.yesterday$Total_reported)

cases.today <- aggregate(Total_reported ~ Date_of_publication, data = dat.today, FUN = sum)
cases.yesterday <- aggregate(Total_reported ~ Date_of_publication, data = dat.yesterday, FUN = sum)
df.cases.new <- merge(cases.today,cases.yesterday,by="Date_of_publication")
df.cases.new$diff <- df.cases.new$Total_reported.x - df.cases.new$Total_reported.y

new.infection <- last(cases.today$Total_reported)
corrections.cases <- sum(df.cases.new$diff)

## Hospitals
net.hospitals <- sum(dat.today$Hospital_admission) - sum(dat.yesterday$Hospital_admission)

hospital.today <- aggregate(Hospital_admission ~ Date_of_publication, data = dat.today, FUN = sum)
hospital.yesterday <- aggregate(Hospital_admission ~ Date_of_publication, data = dat.yesterday, FUN = sum)
df.hospital.new <- merge(hospital.today,hospital.yesterday,by="Date_of_publication")
df.hospital.new$diff <- df.hospital.new$Hospital_admission.x-df.hospital.new$Hospital_admission.y

new.hospitals <- last(hospital.today$Hospital_admission)
corrections.hospitals <- sum(df.hospital.new$diff)

## Deaths
net.deaths <- sum(dat.today$Deceased) - sum(dat.yesterday$Deceased)

death.today <- aggregate(Deceased ~ Date_of_publication, data = dat.today, FUN = sum)
death.yesterday <- aggregate(Deceased ~ Date_of_publication, data = dat.yesterday, FUN = sum)
df.death.new <- merge(death.today,death.yesterday,by="Date_of_publication")
df.death.new$diff <- df.death.new$Deceased.x-df.death.new$Deceased.y

new.deaths <- last(death.today$Deceased)
corrections.deaths <- sum(df.death.new$diff)


corrections.all <- as.data.frame(cbind(new.infection,corrections.cases, net.infection,new.hospitals,corrections.hospitals, 
                                       net.hospitals,new.deaths,corrections.deaths,net.deaths))

corrections.all$date <- as.Date(Sys.Date())

filename <- paste0("corrections/corrections_per_day/corrections-",Sys.Date(),'.csv')
write.csv(corrections.all, file = filename)

temp = list.files(path = "corrections/corrections_per_day/",pattern="*.csv", full.names = T)
myfiles = lapply(temp, read.csv)
corrections.perday <- dplyr::bind_rows(myfiles)
corrections.perday$positive_7daverage <- round(frollmean(corrections.perday[,"new.infection"],7),0) # Calculate 7-day average (based on newly reported infections, gross number)

write.csv(corrections.perday, file = "corrections/corrections_perday.csv", row.names = FALSE)
