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
  dplyr::select(Municipality_name, date, Total_reported) # Select municipality, cases reported

dat.wide <- reshape(dat, direction="wide", # Reshape file into wide format -- columns will be dates which report total cases on date
                    timevar="date",
                    idvar="Municipality_name")

dat.wide$Municipality_name <- recode(dat.wide$Municipality_name, "SÃºdwest-FryslÃ¢n" = "Súdwest-Fryslân", 
                                     "Noardeast-FryslÃ¢n" = "Noardeast-Fryslân")
mun.pop <- read.csv("misc/municipalities-population.csv")
dat.wide <- merge(mun.pop,dat.wide, by = "Municipality_name", all.y=TRUE)
write.csv(dat.wide, file = "data/municipality-totals.csv")

dat.today.wide <- transmute(dat.wide,
  municipality = Municipality_name,
  d0 = dat.wide[,ncol(dat.wide)], # today
  d1 = dat.wide[,ncol(dat.wide)-1], # yesterday
  d7 = dat.wide[,ncol(dat.wide)-7], # last week
  d8 = dat.wide[,ncol(dat.wide)-8], # yesterday's last week
  d14 = dat.wide[,ncol(dat.wide)-14], # 2 weeks back
  july1 = dat.wide$`Total_reported.2020-07-01`, # july 1st
  july1_normalized = pmin(d14, july1), 
  current = d0-july1_normalized,
  increase_day = d0-d1, # Calculate increase since last day
  increase_week = d0-d7, # Calculate increase since last week
  population,
  rel_increase_day = increase_day / population * 100000,
  rel_increase_week = increase_week / population * 100000,
  color = ifelse( (d1 - d8) <= 0 & (d0 - d7) > 0, "💥",
          ifelse( rel_increase_week > 50, "🛑", 
          ifelse( rel_increase_week > 5, "🟧",
          ifelse( rel_increase_week > 0, "🟡",
                                         "✅"
  ))))
)

dat.today <- select( dat.today.wide,
  current,
  color,
  municipality,
  increase_day,
  increase_week,
)

write.csv(dat.today.wide, file = "data/municipality-today-detailed.csv")
write.csv(dat.today, file = "data/municipality-today.csv")


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


