const.regenerate <- FALSE; # Overwrite all historic values with the latest file
const.download <- TRUE; # Download the latest csv
const.filename <- "data/nice_by_municipality.csv" ## Filename for daily data municipalities

if (const.download) {
  rivm.hospital <- fread("https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames.csv", sep=";")
  last_date <- as.Date(last(rivm.hospital$Date_of_report))
  filename.hospital.raw <- paste0("raw-data-archive/municipal-hospital-datasets/rivm_hospital_", last_date ,".csv") ## Filename for daily dat.todaya municipalities
  fwrite(rivm.hospital, file=filename.hospital.raw,row.names = F)
  filename.hospital.compressed <- paste0("data-rivm/municipal-hospital-datasets/rivm_hospital_", last_date ,".csv.gz") ## Filename for daily dat.todaya municipalities
  fwrite(rivm.hospital, file=filename.hospital.compressed,row.names = F)
  rm(rivm.hospital, last_date, filename.hospital.raw)
}

temp = list.files(path = "data-rivm/municipal-hospital-datasets/",pattern="*.csv.gz", full.names = T) ## Pull names of all available dat.todayafiles
dat.today <- read.csv(last(temp), fileEncoding = "UTF-8") ## Take last filename from the folder, load csv
dat.yesterday <- read.csv(head(tail(temp, n=2), n=1), fileEncoding = "UTF-8")
rm(temp)

dat.today$date <- anydate(dat.today$Date_of_statistics)
dat.yesterday$date <- anydate(dat.yesterday$Date_of_statistics)
last_date <- last(dat.today$Date_of_statistics)

list(
  dat.today=dat.today, 
  dat.yesterday=dat.yesterday
) %>%
lapply(function(dat) {
  dat <- dat %>%
    group_by(
      Municipality_code
    ) %>%
    mutate(
      Hospital_admission_cum = cumsum(Hospital_admission),
      .after = Hospital_admission
    ) %>%
    select(
      date, 
      Municipality_name, 
      Municipality_code,
      Hospital_admission_cum
    ) %>%
    pivot_wider( 
      values_from = Hospital_admission_cum,
      names_from = date,
      values_fill = 0
    ) %>%
    arrange(
      Municipality_code == "", 
      Municipality_code,
      Municipality_name
    )

  dat[dat$Municipality_code=="GM0164", "Municipality_name"] <- "Hengelo"
  return(dat)
}) %>%
list2env(.GlobalEnv)

if (!const.regenerate){
  dat.yesterday[, last_date] <- dat.today[, last_date]
  dat.today <- dat.yesterday
}

write.csv(
  dat.today, 
  file = const.filename,
  row.names = F,
  fileEncoding = "UTF-8"
)

rm(
  last_date,
  dat.today, 
  dat.yesterday,
  const.regenerate,
  const.download,
  const.filename
)
