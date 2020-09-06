require(tidyverse)
require(data.table)

temp = list.files(path = "data-rivm/municipal-datasets/",pattern="*.csv", full.names = T) ## Pull names of all available datafiles

dat <- read.csv(last(temp), ) ## Take last filename from the folder, load csv
dat$date <- as.Date(dat$Date_of_report) ## character into Date class
used_date <- as.Date(last(dat$Date_of_report))

dat.unknown <- dat %>%
  filter(Municipality_code == "")  %>%
  group_by(date) %>%
  summarise(
    Municipality_name = 'Unknown',
    Municipality_code = '',
    Total_reported = sum(Total_reported),
    .groups = 'drop_last'
  )

dat.total <- dat %>%
  group_by(date) %>%
  summarise(
    Municipality_name = 'Netherlands',
    Municipality_code = '',
    Total_reported = sum(Total_reported),
    .groups = 'drop_last'
  )

dat <- dat %>%
  filter(Municipality_code != "") %>% # Filter observations without municipal name
  select(Municipality_name, Municipality_code,date, Total_reported) %>% # Select municipality, cases reported
  rbind(dat.total) %>%
  rbind(dat.unknown)
  

rm(dat.unknown, dat.total)

dat$Municipality_name <- recode(dat$Municipality_name, 
  "SÃºdwest-FryslÃ¢n" = "Súdwest-Fryslân", 
  "Noardeast-FryslÃ¢n" = "Noardeast-Fryslân"
)

dat.wide <- reshape(dat, direction="wide", # Reshape file into wide format -- columns will be dates which report total cases on date
                    timevar="date",
                    idvar=c("Municipality_name","Municipality_code"))

mun.pop <- read.csv("misc/municipalities-population.csv") %>%
  select(Municipality_code, population)

dat.wide <- merge(mun.pop, dat.wide, by = "Municipality_code", all.y=TRUE)
dat.wide[dat.wide$Municipality_name=="Netherlands", "population"] <- 17445629
write.csv(dat.wide, file = "data/municipality-totals.csv")

dat.lowest <- dat %>%
  filter(date >= as.Date('2020-08-01')) %>%
  group_by(Municipality_name) %>%
  slice(which.min(Total_reported)) %>%
  arrange(match(Municipality_name, c("Total", "Nederland")), Municipality_code)

# create method to convert a rel_increase to traffic light
convert_to_trafficlight <- function(rel_increase) {
  trafficlight <- 
    ifelse( rel_increase >= 50, "🛑",
    ifelse( rel_increase > 5,   "🟧",
    ifelse( rel_increase > 0,   "🟡",
                                "✅"
  )))
  return(trafficlight)
}

calc_growth_increase <- function(increase_7d, increase_14d){
  growth <- 
    ifelse( 
      increase_14d-increase_7d <= 0,
      increase_7d * 100,
      ((increase_7d / (increase_14d - increase_7d)) - 1 ) * 100
  )
  return(growth) 
}

increase_growth_to_arrows <- function(increase_growth) {
  arrows <- 
    ifelse( increase_growth > 100,  "⬆️⬆️",
    ifelse( increase_growth > 1,     "⬆️",
    #ifelse( increase_growth <= -100, "⬇️⬇️",
    ifelse( increase_growth < -1,    "⬇️",
    "-"
  )))#)
  return(arrows)
}

dat.today.wide <- transmute(dat.wide,
  municipality = Municipality_name,
  Municipality_code = Municipality_code, 
  date = used_date,
  d0 = dat.wide[,ncol(dat.wide)], # today
  d1 = dat.wide[,ncol(dat.wide)-1], # yesterday
  d7 = dat.wide[,ncol(dat.wide)-7], # last week
  d8 = dat.wide[,ncol(dat.wide)-8], # yesterday's last week
  d14 = dat.wide[,ncol(dat.wide)-14], # 2 weeks back
  aug1 = dat.wide$`Total_reported.2020-08-01`, # august 1st
  lowest_since_aug1 = dat.lowest$`Total_reported`,
  lowest_since_aug1_date = dat.lowest$`date`,
  current = d0-lowest_since_aug1,
  increase_1d = d0-d1, # Calculate increase since last day
  increase_7d = d0-d7, # Calculate increase in 7 days
  increase_14d = d0-d14, # Calculate increase in 14 days
  increase_growth = calc_growth_increase(increase_7d, increase_14d), # Compare growth of last 7 days vs 7 days before,
  growth = increase_growth_to_arrows(increase_growth),
  population,
  rel_increase_1d = increase_1d / population * 100000,
  rel_increase_7d = increase_7d / population * 100000,
  color = convert_to_trafficlight(rel_increase_7d),
  color_incl_new = ifelse( (d1 - d8) <= 0 & (d0 - d7) > 0, "💥", color),
  color_yesterday = convert_to_trafficlight( (d1 - d8)/ population * 100000),
  color_lastweek = convert_to_trafficlight( (d7 - d14)/ population * 100000)
)

dat.today <- dat.today.wide %>%
  filter(Municipality_code != "") %>%
  arrange(municipality) %>%
  select(
    current,
    color_incl_new,
    municipality,
    increase_1d,
    increase_7d,
    growth,
  )

dat.totals.growth <- dat.today.wide %>%
  filter(Municipality_code != "") %>%
  group_by(growth) %>%
  summarise(d0 = n(), .groups = 'drop_last') %>%
  arrange(match(growth, c("⬆️⬆️","⬆️","-","⬇️⬇️","⬇️")))
  
dat.totals.color <- dat.today.wide %>%
  filter(Municipality_code != "") %>%
  group_by(color) %>%
  summarise(d0 = n(), .groups = 'drop_last')

dat.totals.color_yesterday <- dat.today.wide %>%
  filter(Municipality_code != "") %>%
  group_by(color_yesterday) %>%
  summarise(d1 = n(), .groups = 'drop_last') %>%
  rename(color = color_yesterday)

dat.totals.color_lastweek <- dat.today.wide %>%
  filter(Municipality_code != "") %>%
  group_by(color_lastweek) %>%
  summarise(d7 = n(), .groups = 'drop_last') %>%
  rename(color = color_lastweek)

dat.totals.color <- dat.totals.color %>%
  merge(dat.totals.color_yesterday, by = "color", all.y=TRUE) %>%
  merge(dat.totals.color_lastweek, by = "color", all.y=TRUE) %>%
  arrange(match(color, c("✅","🟡","🟧","🛑")))

rm(dat.totals.color_yesterday, dat.totals.color_lastweek)

dat.totals.color <- mutate(dat.totals.color,
  increase_1d = d0-d1, # Calculate increase since last day
  increase_7d = d0-d7, # Calculate increase in 7 days
)

write.csv(dat.today.wide, file = "data/municipality-today-detailed.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.today, file = "data/municipality-today.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.totals.growth, file = "data/municipality-totals-growth.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.totals.color, file = "data/municipality-totals-color.csv",row.names = F, fileEncoding = "UTF-8")


# rmdshot("workflow/daily_municipality.Rmd", "plots/list_municipality_1.png", delay = 1)

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
