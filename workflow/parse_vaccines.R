## Vaccines delivered
dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
vaccines_delivery <- as.data.frame(dat$vaccine_delivery$values)
vaccines_delivery$date <- as.Date(as.POSIXct(vaccines_delivery$date_of_insertion_unix, origin="1970-01-01"))
filename.daily.vaccins.delivered <- paste0("data-rivm/vaccines-delivered/rivm_daily_vaccines_",Sys.Date(),".csv")
write.csv(vaccines_delivery, file = filename.daily.vaccins.delivered, row.names = F)

## Vaccines used

df.care.institutions <- dat$vaccine_administered_care_institutions$values
colnames(df.care.institutions) <- c("date","vaccines_administered_estimated_carehomes","report_date_carehomes")
df.ggd <- dat$vaccine_administered_ggd$values
colnames(df.ggd) <- c("date","vaccines_administered_ggd","report_date_ggd")
df.hospitals <- dat$vaccine_administered_hospitals$values
colnames(df.hospitals) <- c("date","vaccines_administered_hospital","report_date_hospitals")
df.total <- dat$vaccine_administered_total$values
colnames(df.total) <- c("vaccines_administered_estimated","vaccines_administered","date","report_date_total")
df.doctors <- dat$vaccine_administered_doctors$values
colnames(df.doctors) <- c("date","vaccines_administered_doctors","report_date_doctors")

daily_vaccin_datalist <- list(df.care.institutions,df.ggd,df.hospitals, df.doctors,df.total)

vaccine.data <- Reduce(
  function(x, y, ...) merge(x, y, by="date",all.x = TRUE, ...),
  daily_vaccin_datalist
)

# Build vaccine data
vaccine.data <- vaccine.data[,c("date","vaccines_administered_estimated_carehomes","vaccines_administered_ggd","vaccines_administered_hospital",
                                "vaccines_administered_estimated","vaccines_administered_doctors","vaccines_administered")]
vaccine.data$date <- as.Date(as.POSIXct(vaccine.data$date, origin="1970-01-01"))
vaccines_by_day <- vaccine.data[order(vaccine.data$date),]

last.date <- last(vaccines_by_day$date)

# Write vaccine data file for day
filename.daily.vaccins <- paste0("data-rivm/vaccines-per-day/rivm_daily_vaccines_",last.date,".csv")
write.csv(vaccines_by_day, file = filename.daily.vaccins, row.names = F)

## Merge all daily vaccinedata

temp = list.files(path = "data-rivm/vaccines-per-day/",pattern="*.csv", full.names = T) ## Fetch all day files
myfiles = lapply(temp, fread) ## Load all day files

vaccine_data <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

write.csv(vaccine_data, file = "data/vaccines_by_day.csv",row.names = F) ## Write file with aggregate data per day