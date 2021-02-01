require(rvest)


## Vaccines delivered

dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
vaccines_delivery <- as.data.frame(dat$vaccine_delivery$values)
vaccines_delivery$date <- as.Date(as.POSIXct(vaccines_delivery$date_of_insertion_unix, origin="1970-01-01"))
filename.daily.vaccins.delivered <- paste0("data-rivm/vaccines-delivered/rivm_daily_vaccines_",Sys.Date(),".csv")
write.csv(vaccines_delivery, file = filename.daily.vaccins.delivered, row.names = F)

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
vaccins.toegediend_geschat <- html_nodes(webpage, ".hDwZKX")
text_vaccins_dashboard <- as.data.frame(html_text(vaccins.toegediend_geschat)[20])

vaccins_totaal_toegediend <- parse_number(str_match(text_vaccins_dashboard, ".*cijferverantwoording.(.*?).gezet door GGD'en.*")[2])*1000

## Vaccines used estimated
vaccins.toegediend_geschat <- html_nodes(webpage, ".hBabwA")
vaccins.toegediend.totaal_geschat <- parse_number(html_text(vaccins.toegediend_geschat)[1])*1000

vaccins.per.prikker <- html_nodes(webpage, ".bctkLi")
vaccins.ggd.geschat <- as.numeric(html_text(vaccins.per.prikker)[1])*1000
vaccins.ziekenhuizen.geschat <- as.numeric(html_text(vaccins.per.prikker)[2])*1000
vaccins.zorginstellingen.geschat <- as.numeric(html_text(vaccins.per.prikker)[3])*1000

vaccins.dailydata <- data.frame(as.Date(Sys.Date()),vaccins_totaal_toegediend,vaccins.toegediend.totaal_geschat,vaccins.geleverd.totaal,vaccins.ggd.geschat,vaccins.ziekenhuizen.geschat,vaccins.zorginstellingen.geschat) ## Calculate totals for cases, hospitalizations, deaths
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

## Parse daily percentage positive tests - national ##

dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
tested_daily <- as.data.frame(dat$tested_ggd_daily[1])
tested_daily$date <- as.Date(as.POSIXct(tested_daily$values.date_unix, origin="1970-01-01"))
tested_daily$pos.rate.3d.avg <- round(frollmean(tested_daily[,"values.infected_percentage"],3),1)
tested_daily$tests.7d.avg <- round(frollmean(tested_daily[,"values.tested_total"],7),1)

write.csv(tested_daily, file = "data-dashboards/percentage-positive-daily-national.csv",row.names = F)

## Parse daily percentage positive tests - safety region ##

df.vr.dailytests <- data.frame()

for (i in 1:25) {
  if(i<10){
    db <- fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/json/VR0",i,".json"))
  }else{
    db <- fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/json/VR",i,".json"))
  }
  db <- as.data.frame(db$tested_ggd_daily[1])
  df.vr.dailytests <- rbind(df.vr.dailytests,db)
}
df.vr.dailytests$date <- as.Date(as.POSIXct(df.vr.dailytests$values.date_unix, origin="1970-01-01"))
df.vr.dailytests$pos.rate.3d.avg <- round(frollmean(df.vr.dailytests[,"values.infected_percentage"],3),1)

write.csv(df.vr.dailytests, file = "data-dashboards/percentage-positive-daily-safetyregion.csv",row.names = F)



