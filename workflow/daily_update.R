require(cowplot)
require(tidyverse)
require(rjson)
require(rtweet)
require(data.table)
require(git2r)

twit.auth <- read.csv("twitter_auth.csv")
token <- create_token(app = twit.auth[1,2],
                      consumer_key = twit.auth[2,2],
                      access_token = twit.auth[3,2],
                      consumer_secret = twit.auth[4,2],
                      access_secret = twit.auth[5,2])

Sys.sleep(10)

# Generate Banner
source("workflow/generate_banner.R")

# Parse RIVM, NICE and corrections data
source("workflow/parse_rivm-data.R") ## Run only after new data upload by RIVM at 14:15
source("workflow/parse_municipalities.R")
#source("workflow/parse_nice-data.R")
source("workflow/parse_corrections.R")

## Merge RIVM, NICE and corrections data

rivm_by_day <- read.csv("data/rivm_by_day.csv")
nice_today <- read.csv("data/nice_by_day.csv")
corrections.perday <- read.csv("corrections/corrections_perday.csv")

daily_datalist <- list(rivm_by_day,nice_today,corrections.perday)

all.data <- Reduce(
  function(x, y, ...) merge(x, y, by="date",all.x = TRUE, ...),
  daily_datalist
)

all.data$date <- as.Date(all.data$date)
all.data <- all.data[order(all.data$date),]

write.csv(all.data, file = "data/all_data.csv",row.names = F)

#source("plot_scripts/daily_plots.R")
#source("plot_scripts/daily_maps_plots.R")

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc"); rmarkdown::render('reports/daily_report.Rmd') ## Render daily report
file.copy(from = list.files('reports', pattern="*.pdf",full.names = TRUE), 
          to = paste0("reports/daily_reports/Epidemiologische situatie COVID-19 in Nederland - ",
                                 format((Sys.Date()),'%d')," ",format((Sys.Date()),'%B'),".pdf")) ## Save daily file in archive

all.data <- read.csv("data/all_data.csv")
nice_by_day <- read.csv("data/nice_by_day.csv")

## Push to git

status()

repo <- init()

add(repo, path = "*")

commit(repo, all = T, paste0("Daily (automated) update RIVM and NICE data ",Sys.Date()))

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

push(repo, credentials = git.auth)

## Corrections or not?
text.hosp.corrections <- paste0(ifelse(last(all.data$net.hospitals)>=0," (+"," (-"),abs(last(all.data$net.hospitals))," ivm ",last(all.data$corrections.hospitals)," corr.)")
text.deaths.corrections <- paste0(ifelse(last(all.data$net.deaths)>=0," (+"," (-"),abs(last(all.data$net.deaths))," ivm ",last(all.data$corrections.deaths)," corr.)")

## Build tweets
tweet <- paste0("#COVID19NL statistieken t.o.v. gisteren: 

Positief getest: ",last(all.data$new.infection),"
Totaal: ",last(all.data$cases)," (+",last(all.data$net.infection)," ivm ",last(all.data$corrections.cases)," corr.)

Opgenomen: ",last(all.data$new.hospitals),"
Totaal: ",last(all.data$hospitalization),ifelse(last(all.data$corrections.hospitals)<0,text.hosp.corrections,""),"

Opgenomen op IC*: ",tail(diff(nice_by_day$IC_Cumulative),n=1),"
Huidig: ",last(nice_by_day$IC_Current),"
Totaal: ",tail(nice_by_day$IC_Cumulative,n=1),"
* bewezen of verdacht
(www.stichting-nice.nl)

Overleden: ",last(all.data$new.deaths),"
Totaal: ",last(all.data$deaths),ifelse(last(all.data$corrections.deaths)<0,text.deaths.corrections,""))

tweet

today.date <- paste0("banners/",Sys.Date(),".png")

post_tweet(status = "Dit is een test tweet in het kader van het verder automatiseren van de dagelijkse update: https://github.com/mzelst/covid-19/raw/master/reports/daily_report.pdf")

#post_tweet (status = tweet,media = today.date) ## Post tweet

# Tweet for hospital numbers

tweet2 <- paste0("Update met betrekking tot ziekenhuis-gegevens (data NICE): 

Patiënten verpleegafdeling 
Bevestigd: ",tail(all.data$Hospital_Intake_Proven,n=1),". Verdacht: ",tail(all.data$Hospital_Intake_Suspected, n=1),".

Patiënten IC
Bevestigd: ",tail(all.data$IC_Intake_Proven,n=1),". Verdacht: ",tail(all.data$IC_Intake_Suspected,n=1),".

Grafisch per dag: Het aantal aanwezige patiënten in het ziekenhuis, opnames, besmettingen, en het reproductiegetal.")

# Tweet for graph
#my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
#reply_id <- my_timeline$status_id[1] ## Status ID for reply
#post_tweet(tweet2, media = "plots/plot_daily.png",
 #          in_reply_to_status_id = reply_id) ## Post reply

my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
reply_id <- my_timeline$status_id[1] ## Status ID for reply
post_tweet("Ik heb een start gemaakt met een dagelijks epidemiologisch rapport (work in progress). Hierin vindt u kaarten en tabellen met gegevens per leeftijdsgroep, provincie, en GGD: https://github.com/mzelst/covid-19/raw/master/reports/daily_report.pdf",
          in_reply_to_status_id = reply_id) ## Post reply

#my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
#reply_id <- my_timeline$status_id[1] ## Status ID for reply
#post_tweet("Vergeet ook niet de tweets hieronder van @edwinveldhuizen te checken voor de regionale verschillen en trends.",
  #         in_reply_to_status_id = reply_id) ## Post reply

# Data municipalities per day

rivm.municipalities.perday <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv",sep=";")
filename.municipality.perday <- paste0("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_",Sys.Date(),".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities.perday, file=filename.municipality.perday,row.names = F)

## Push data municipalities per day to repo
add(repo, path = "*")
commit(repo, all = T, paste0("Data municipalities per day ",Sys.Date()))
push(repo, credentials = git.auth)

rm(list=ls()) # Clean environment


