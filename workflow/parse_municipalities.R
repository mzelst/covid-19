require(tidyverse)
require(data.table)

## Test code - Only use latest dataset

temp = list.files(path = "data-rivm/municipal-datasets/",pattern="*.csv", full.names = T) ## Pull names of all available datafiles

dat <- read.csv(last(temp), ) ## Take last filename from the folder, load csv
dat$date <- as.Date(dat$Date_of_report) ## character into Date class
filter.date <- Sys.Date()-28 ## Create filter for last four weeks +1

dat <- dat %>%
  dplyr::filter(Municipality_name != "") %>% # Filter observations without municipal name
#  dplyr::filter(date >= filter.date) %>% # Filter last four weeks
  dplyr::select(Municipality_name, Municipality_code,date, Total_reported) # Select municipality, cases reported

dat$Municipality_name <- recode(dat$Municipality_name, "SÃºdwest-FryslÃ¢n" = "Súdwest-Fryslân",
                                     "Noardeast-FryslÃ¢n" = "Noardeast-Fryslân")

dat.wide <- reshape(dat, direction="wide", # Reshape file into wide format -- columns will be dates which report total cases on date
                    timevar="date",
                    idvar=c("Municipality_name","Municipality_code"))


mun.pop <- read.csv("misc/municipalities-population.csv")
dat.wide <- merge(mun.pop,dat.wide, by = "Municipality_name", all.y=TRUE)
write.csv(dat.wide, file = "data/municipality-totals.csv")

dat.lowest <- dat %>% 
  dplyr::filter(date >= as.Date('2020-07-01')) %>%
  group_by(Municipality_name) %>%
  slice(which.min(Total_reported))

# create method to convert a rel_increase to traffic light
increase_to_trafficlight <- function(rel_increase) {
  trafficlight <- ifelse( rel_increase >= 50, "🛑",
                  ifelse( rel_increase > 5, "🟧",
                  ifelse( rel_increase > 0, "🟡",
                  "✅"
  )))
  return(trafficlight)
}

dat.today.wide <- transmute(dat.wide,
  municipality = Municipality_name,
  d0 = dat.wide[,ncol(dat.wide)], # today
  d1 = dat.wide[,ncol(dat.wide)-1], # yesterday
  d7 = dat.wide[,ncol(dat.wide)-7], # last week
  d8 = dat.wide[,ncol(dat.wide)-8], # yesterday's last week
  d14 = dat.wide[,ncol(dat.wide)-14], # 2 weeks back
  july1 = dat.wide$`Total_reported.2020-07-01`, # july 1st
  lowest_since_july1 = dat.lowest$`Total_reported`,
  lowest_since_july1_date = dat.lowest$`date`,
  current = d0-lowest_since_july1,
  increase_day = d0-d1, # Calculate increase since last day
  increase_week = d0-d7, # Calculate increase since last week
  population,
  rel_increase_day = increase_day / population * 100000,
  rel_increase_week = increase_week / population * 100000,
  color = increase_to_trafficlight(rel_increase_week),
  color_incl_new = ifelse( (d1 - d8) <= 0 & (d0 - d7) > 0, "💥", color),
  color_yesterday = increase_to_trafficlight( (d1 - d8)/ population * 100000),
  color_lastweek = increase_to_trafficlight( (d7 - d14)/ population * 100000)
)

dat.today <- select( dat.today.wide,
  current,
  color_incl_new,
  municipality,
  increase_day,
  increase_week,
)

write.csv(dat.today.wide, file = "data/municipality-today-detailed.csv")
write.csv(dat.today, file = "data/municipality-today.csv")


## Pull municipal data from CBS

#require(cbsodataR)
#require(geojsonio)

#dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c("2020MM06")))
#dat.mun <- dat.mun[,c("RegioS","BevolkingAanHetEindeVanDePeriode_15")]
#colnames(dat.mun) <- c("statcode","populatie")

#dat.mun <- dat.mun %>%
#  dplyr::filter(!is.na(populatie)) %>%
#  dplyr::filter(grepl("GM", statcode))

#gemeentegrenzen <- geojson_read("misc/maps/gemeentegrenzen2020.geojson", what = "sp")
#gemeentes <- gemeentegrenzen@data[,c(2,4)]

#gemeente.stats <- merge(gemeentes, dat.mun, by = "statcode")
#colnames(gemeente.stats) <- c("Municipality_code","Municipality_name","population")
#write.csv(gemeente.stats, file = "misc/municipalities-population.csv")



# Municipality data

#temp = list.files(path = "data-rivm/municipal-datasets/",pattern="*.csv", full.names = T)
#myfiles = lapply(temp, read.csv)

#temp = list.files(path = "data-rivm/municipal-datasets/",pattern="*.csv")

#myfiles <- mapply(cbind, myfiles, "name_dataset"=temp,SIMPLIFY = F)

#datefunction <- function(x) {
#  x[x$Date_of_report == last(x$Date_of_report),]
#} ## Function for cases per day

#res <- lapply(myfiles, datefunction)

#df <- map_dfr(res, ~{
#  .x
#})

#df$date <- as.Date(df$Date_of_report)

#df <- df %>%
#  filter(Municipality_name != "") %>%
#  select(Municipality_name, date, Total_reported)

#data_wide <- reshape(df, direction="wide",
#                     timevar="date",
#                     idvar="Municipality_name")

# Calc diffs

#col.start.diff <- ncol(data_wide)+1

#dates.lead <- names(data_wide)[3:ncol(data_wide)] ## Set lead colnames for diff
#dates.trail <- names(data_wide)[2:(ncol(data_wide)-1)] ## Set trail colnames for diff

# Calculate moving difference between cases per day
#data_wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- data_wide[dates.lead] - data_wide[dates.trail]

#week <- last(colnames(data_wide), n = 7)

#data_wide$weeksum <- rowSums(data_wide[,week])

# rm(list=ls())
