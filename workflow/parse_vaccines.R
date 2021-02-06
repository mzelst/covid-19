## Vaccines delivered
dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
vaccines_delivery <- as.data.frame(dat$vaccine_delivery$values)
vaccines_delivery$date <- as.Date(as.POSIXct(vaccines_delivery$date_of_insertion_unix, origin="1970-01-01"))
filename.daily.vaccins.delivered <- paste0("data-rivm/vaccines-delivered/rivm_daily_vaccines_",Sys.Date(),".csv")
write.csv(vaccines_delivery, file = filename.daily.vaccins.delivered, row.names = F)
sum(vaccines_delivery[,c("astra_zeneca","pfizer","cure_vac","janssen","moderna","sanofi")])

## Vaccines used

## Locale pulled in from the dashboard repository, with most recent version in branch `develop`
locale_u <- "https://raw.githubusercontent.com/minvws/nl-covid19-data-dashboard/develop/packages/app/src/locale/nl.json"

locale_dat <- fromJSON(txt = locale_u)
vaccinaties_data_locale <- locale_dat$vaccinaties$data
## Vaccines delivered (dashboard webscrape)
vaccins.geleverd.totaal <- vaccinaties_data_locale$kpi_expected_delivery$value

## Vaccines used (reported)

u <- "https://coronadashboard.rijksoverheid.nl/landelijk/vaccinaties"
webpage <- read_html(u)

## Vaccines used estimated
vaccins.toegediend_geschat <- html_nodes(webpage, ".hBabwA")
vaccins.toegediend.totaal_geschat <- parse_number(html_text(vaccins.toegediend_geschat)[1])*1000

vaccins.per.prikker <- html_nodes(webpage, ".bctkLi")
vaccins.ggd <- as.numeric(html_text(vaccins.per.prikker)[1])*1000
vaccins.ziekenhuizen <- as.numeric(html_text(vaccins.per.prikker)[2])*1000
vaccins.zorginstellingen.geschat <- as.numeric(html_text(vaccins.per.prikker)[3])*1000

vaccins_totaal_toegediend <- vaccins.ggd + vaccins.ziekenhuizen

vaccins.dailydata <- data.frame(as.Date(Sys.Date()),vaccins_totaal_toegediend,vaccins.toegediend.totaal_geschat,vaccins.geleverd.totaal,vaccins.ggd,vaccins.ziekenhuizen,vaccins.zorginstellingen.geschat) ## Calculate totals for cases, hospitalizations, deaths
names(vaccins.dailydata) <- c("date","vaccines_administered","vaccines_administered_estimated","vaccines_expected_6weeks","vaccines_administered_ggd","vaccines_administered_estimated_hospital","vaccines_administered_estimated_carehomes")

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