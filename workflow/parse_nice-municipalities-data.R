require(tidyverse)
require(data.table)

const.regenerate <- FALSE; # Overwrite all historic values with the latest file
const.download <- TRUE; # Download the latest csv
const.filename <- "data/nice_by_municipality.csv" ## Filename for daily data municipalities

if (const.download) {
  rivm.hospital <- read.csv("https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames.csv", sep=";")
  last_date <- as.Date(last(rivm.hospital$Date_of_report))
  filename.hospital <- paste0("data-rivm/municipal-hospital-datasets/rivm_hospital_", last_date ,".csv") ## Filename for daily data municipalities
  write.csv(rivm.hospital, file=filename.hospital,row.names = F)
  rm(rivm.hospital, last_date, filename.hospital )
}

temp = list.files(path = "data-rivm/municipal-hospital-datasets/",pattern="*.csv", full.names = T) ## Pull names of all available datafiles
dat <- read.csv(last(temp), fileEncoding = "UTF-8") ## Take last filename from the folder, load csv
rm(temp)

dat$date <- as.Date(dat$Date_of_statistics)
last_date <- last(dat$Date_of_statistics)

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

if (!const.regenerate){
  current <- read.csv(
      const.filename, 
      fileEncoding = "UTF-8", 
      check.names = FALSE
    ) %>%
    arrange(
      Municipality_code == "", 
      Municipality_code,
      Municipality_name
    )
  
  current[, last_date] <- dat[, last_date]
  dat <- current
}

write.csv(
  dat, 
  file = const.filename,
  row.names = F,
  fileEncoding = "UTF-8"
)

rm(
  dat, 
  const.regenerate,
  const.download,
  const.filename
)
