require(cowplot)
require(tidyverse)
require(rjson)
require(rtweet)
require(data.table)
get_token()

rivm.data <- read.csv("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv", sep=";") ## Read in data with all cases until today

# Parse RIVM, NICE and corrections data
source("workflow/parse_rivm-data.R") ## Run only after new data upload by RIVM at 14:15
source("workflow/parse_municipalities.R")
source("workflow/parse_nice-data.R")
source("workflow/parse_corrections.R")

## Merge RIVM, NICE and corrections data

rivm_by_day <- read.csv("data/rivm_by_day.csv")
nice_today <- read.csv("data-nice/nice-today.csv")
corrections.perday <- read.csv("corrections/corrections_perday.csv")
daily_datalist <- list(rivm_by_day,nice_today,corrections.perday)

all.data <- Reduce(
  function(x, y, ...) merge(x, y, by="date",all.x = TRUE, ...),
  daily_datalist
)

all.data$date <- as.Date(all.data$date)
all.data <- all.data[order(all.data$date),]

all.data$positive_7daverage <- round(frollmean(all.data[,"new.infection"],7),0) # Calculate 7-day average (based on newly reported infections, gross number)

write.csv(all.data, file = "data/all_data.csv")

source("plot_scripts/daily_plots.R")
source("plot_scripts/daily_maps_plots.R")

all.data <- read.csv("data/all_data.csv")
nice_by_day <- read.csv("data/nice_by_day.csv")

# ## Build tweets
# tweet <- paste0("#COVID19NL statistieken t.o.v. gisteren: 
# 
# Positief getest: ",last(all.data$new.infection),"
# Totaal: ",last(all.data$cases)," (+",last(all.data$net.infection)," ivm ",last(all.data$corrections.cases)," corr.)
# 
# Opgenomen: ",last(all.data$new.hospitals),"
# Totaal: ",last(all.data$hospitalization),ifelse(last(all.data$net.hospitals)>=0," (+"," (-"),abs(last(all.data$net.hospitals))," ivm ",last(all.data$corrections.hospitals)," corr.)
# 
# Opgenomen op IC: ",tail(diff(nice_by_day$IC_Cumulative),n=1),"
# Totaal: ",tail(nice_by_day$IC_Cumulative,n=1),"
# 
# Overleden: ",last(all.data$new.deaths),"
# Totaal: ",last(all.data$deaths),ifelse(last(all.data$net.deaths)>=0," (+"," (-"),abs(last(all.data$net.deaths))," ivm ",last(all.data$corrections.deaths)," corr.)")
# 
# tweet
# 
# today.date <- paste0("banners/",Sys.Date(),".png")
# 
# post_tweet (status = tweet,media = today.date) ## Post tweet
# 
# # Tweet for hospital numbers
# 
# tweet2 <- paste0("Update met betrekking tot ziekenhuis-gegevens (data NICE): 
# 
# Patiënten verpleegafdeling 
# Bevestigd: ",tail(all.data$Hospital_Intake_Proven,n=1),". Verdacht: ",tail(all.data$Hospital_Intake_Suspected, n=1),".
# 
# Patiënten IC
# Bevestigd: ",tail(all.data$IC_Intake_Proven,n=1),". Verdacht: ",tail(all.data$IC_Intake_Suspected,n=1),".
# 
# Grafisch per dag: Het aantal aanwezige patiënten in het ziekenhuis, opnames, aantal besmettingen, en het reproductiegetal.")
# 
# # Tweet for graph
# my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
# reply_id <- my_timeline$status_id[1] ## Status ID for reply
# post_tweet(tweet2, media = "plots/plot_daily.png",
#            in_reply_to_status_id = reply_id) ## Post reply
# 
# my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
# reply_id <- my_timeline$status_id[1] ## Status ID for reply
# post_tweet("Vergeet ook niet de tweets hieronder van @edwinveldhuizen te checken voor de regionale verschillen en trends. Wellicht zijn deze nog belangrijker dan de landelijke cijfers.",
#            in_reply_to_status_id = reply_id) ## Post reply
# 
# my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
# reply_id <- my_timeline$status_id[1] ## Status ID for reply
# post_tweet("De komende tijd zal ik proberen de dagelijkse updates uitgebreider te maken. De kaarten hieronder laten het aantal besmettingen zien van afgelopen dag (links) en afgelopen week (rechts).",
#            media = "plots/plot_daily_maps.png",
#            in_reply_to_status_id = reply_id) ## Post reply
# 
# my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
# reply_id <- my_timeline$status_id[1] ## Status ID for reply
# post_tweet("Reminder: ik heb staycation en zal mijn mentions sporadisch checken. Bij inhoudelijke vragen kunt u een DM sturen (ik hanteer een zero-tolerance policy voor niet-inhoudelijke vragen). Stay safe! https://twitter.com/mzelst/status/1285917046698647553",
#            in_reply_to_status_id = reply_id) ## Post reply
# 
# rm(list=ls()) # Clean environment
