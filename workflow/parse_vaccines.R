require(rvest)


## Vaccines delivered

dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
vaccines_delivery <- as.data.frame(dat$vaccine_delivery$values)
vaccines_delivery$date <- as.Date(as.POSIXct(vaccines_delivery$date_of_insertion_unix, origin="1970-01-01"))
filename.daily.vaccins.delivered <- paste0("data-rivm/vaccines-delivered/rivm_daily_vaccines_",Sys.Date(),".csv")
write.csv(vaccines_delivery, file = filename.daily.vaccins.delivered, row.names = F)

## Vaccines used

u <- "https://coronadashboard.rijksoverheid.nl/landelijk/vaccinaties"

webpage <- read_html(u)
vaccins.toegediend <- html_nodes(webpage, ".hBabwA")
vaccins.toegediend.totaal <- parse_number(html_text(vaccins.toegediend)[1])*1000
vaccins.geleverd.totaal.txt <- html_text(vaccins.toegediend)[2]
vaccins.geleverd.totaal <- scan(text=vaccins.geleverd.totaal.txt, sep = ".")
vaccins.geleverd.totaal <- as.numeric(paste0(vaccins.geleverd.totaal[1],vaccins.geleverd.totaal[2],vaccins.geleverd.totaal[3]))

vaccins.per.prikker <- html_nodes(webpage, ".gUeNWf")
vaccins.ggd <- as.numeric(html_text(vaccins.per.prikker)[1])*1000
vaccins.ziekenhuizen <- as.numeric(html_text(vaccins.per.prikker)[2])*1000
vaccins.zorginstellingen <- as.numeric(html_text(vaccins.per.prikker)[3])*1000

vaccins.dailydata <- data.frame(as.Date(Sys.Date()),vaccins.toegediend.totaal,vaccins.geleverd.totaal,vaccins.ggd,vaccins.ziekenhuizen,vaccins.zorginstellingen) ## Calculate totals for cases, hospitalizations, deaths
names(vaccins.dailydata) <- c("date","vaccines_administered","vaccines_expected_6weeks","vaccines_administered_ggd","vaccines_administered_hospital","vaccines_administered_carehomes")

filename.daily.vaccins <- paste0("data-rivm/vaccines-per-day/rivm_daily_vaccines_",Sys.Date(),".csv")

write.csv(vaccins.dailydata, file = filename.daily.vaccins, row.names = F)

# 
temp = list.files(path = "data-rivm/vaccines-per-day/",pattern="*.csv", full.names = T) ## Fetch all day files
myfiles = lapply(temp, read.csv) ## Load all day files

vaccines_by_day <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

vaccines_by_day$date <- as.Date(vaccines_by_day$date)
vaccines_by_day <- vaccines_by_day[order(vaccines_by_day$date),]

write.csv(vaccines_by_day, file = "data/vaccines_by_day.csv",row.names = F) ## Write file with aggregate data per day


