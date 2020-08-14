require(tidyverse)
require(data.table)

## Test code - Only use latest dataset

temp = list.files(path = "data-rivm/municipal-datasets/",pattern="*.csv", full.names = T) ## Pull names of all available datafiles

dat <- read.csv(last(temp), ) ## Take last filename from the folder, load csv
dat$date <- as.Date(dat$Date_of_report) ## character into Date class
filter.date <- Sys.Date()-28 ## Create filter for last four weeks +1

dat <- dat %>%
  dplyr::filter(Municipality_name != "") %>% # Filter observations without municipal name
  dplyr::filter(date >= filter.date) %>% # Filter last four weeks 
  dplyr::select(Municipality_name, Municipality_code,date, Total_reported) # Select municipality, cases reported

dat.wide <- reshape(dat, direction="wide", # Reshape file into wide format -- columns will be dates which report total cases on date
                    timevar="date",
                    idvar=c("Municipality_name","Municipality_code"))

dat.wide$increase <- dat.wide[,ncol(dat.wide)]-dat.wide[,(ncol(dat.wide)-1)] # Calculate increase since last day 
dat.wide$increase.week <- dat.wide[,(ncol(dat.wide)-1)]-dat.wide[,(ncol(dat.wide)-8)] # Calculate increase since last week

dat.wide$Municipality_name <- recode(dat.wide$Municipality_name, "SÃºdwest-FryslÃ¢n" = "Súdwest-Fryslân", 
                                     "Noardeast-FryslÃ¢n" = "Noardeast-Fryslân")


mun.pop <- read.csv("misc/municipalities-population.csv")
dat.wide <- merge(mun.pop,dat.wide, by = "Municipality_code", all.y=TRUE)
dat.wide$rel.increase <- dat.wide$increase/dat.wide$population*100000
dat.wide$rel.increase.week <- dat.wide$increase.week/dat.wide$population*100000

write.csv(dat.wide, file = "data/municipality-today.csv",row.names = F)


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

rm(list=ls())


