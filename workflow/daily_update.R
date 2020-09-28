pull(repo)

# Generate Banner
source("workflow/generate_banner.R")

# Parse RIVM, NICE and corrections data
source("workflow/parse_rivm-data.R") ## Run only after new data upload by RIVM at 14:15
source("workflow/parse_municipalities.R")
source("workflow/parse_nice-data.R")
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

source("plot_scripts/daily_plots.R")
#source("plot_scripts/daily_maps_plots.R")

rm(list=ls()) # Clean environment

all.data <- read.csv("data/all_data.csv")
nice_by_day <- read.csv("data/nice_by_day.csv")

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

post_tweet (status = tweet,media = (paste0("banners/",Sys.Date(),".png"))) ## Post tweet

# Tweet for hospital numbers - Data NICE ####

temp = tail(list.files(path = "data-nice/data-nice-json/",pattern="*.csv", full.names = T),2)
myfiles = lapply(temp, read.csv)

dat.today <- as.data.frame(myfiles[2])
dat.yesterday <- as.data.frame(myfiles[1])

Verpleeg_Opname_Bevestigd <- sum(dat.today$Hospital_Intake_Proven) - sum(dat.yesterday$Hospital_Intake_Proven)
Verpleeg_Opname_Verdacht <- sum(dat.today$Hospital_Intake_Suspected) - sum(dat.yesterday$Hospital_Intake_Suspected)

IC_Opname_Bevestigd <- sum(dat.today$IC_Intake_Proven) - sum(dat.yesterday$IC_Intake_Proven)
IC_Opname_Verdacht <- sum(dat.today$IC_Intake_Suspected) - sum(dat.yesterday$IC_Intake_Suspected)

Verpleeg_Huidig_Toename <- last(dat.today$Hospital_Currently) - last(dat.yesterday$Hospital_Currently)
IC_Huidig_Toename <- last(dat.today$IC_Current) - last(dat.yesterday$IC_Current)

hospital.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-cumulative/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

sign.hosp.nice <- paste0(ifelse(Verpleeg_Huidig_Toename>=0," (+"," ("))
sign.ic.nice <- paste0(ifelse(IC_Huidig_Toename>=0," (+"," ("))

tweet2 <- paste0("#COVID19NL statistieken t.o.v. gisteren (data NICE): 

Patiënten verpleegafdeling 
Bevestigd: ",Verpleeg_Opname_Bevestigd,"
Verdacht: ",Verpleeg_Opname_Verdacht,"
Huidig: ",last(dat.today$Hospital_Currently),sign.hosp.nice,Verpleeg_Huidig_Toename,")
Totaal: ",last(hospital.cumulative$value),"

Patiënten IC
Bevestigd: ",IC_Opname_Bevestigd,"
Verdacht: ",IC_Opname_Verdacht,"
Huidig: ",last(dat.today$IC_Current),sign.ic.nice,IC_Huidig_Toename,")
Totaal: ",last(dat.today$IC_Cumulative))

tweet2

# Tweet for report ####
my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
reply_id <- my_timeline$status_id[1] ## Status ID for reply
post_tweet(tweet2, media = "plots/plot_daily.png",
          in_reply_to_status_id = reply_id) ## Post reply

my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
reply_id <- my_timeline$status_id[1] ## Status ID for reply
post_tweet("Ik heb een start gemaakt met een dagelijks epidemiologisch rapport (work in progress). Hierin vindt u kaarten en tabellen met gegevens per leeftijdsgroep, provincie, en GGD: https://github.com/mzelst/covid-19/raw/master/reports/daily_report.pdf",
          in_reply_to_status_id = reply_id) ## Post reply

#my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
#reply_id <- my_timeline$status_id[1] ## Status ID for reply
#post_tweet("Vergeet ook niet de tweets hieronder van @edwinveldhuizen te checken voor de regionale verschillen en trends.",
  #         in_reply_to_status_id = reply_id) ## Post reply

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc"); rmarkdown::render('reports/daily_report.Rmd') ## Render daily report
file.copy(from = list.files('reports', pattern="*.pdf",full.names = TRUE), 
          to = paste0("reports/daily_reports/Epidemiologische situatie COVID-19 in Nederland - ",
                      format((Sys.Date()),'%d')," ",format((Sys.Date()),'%B'),".pdf")) ## Save daily file in archive

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Daily (automated) update RIVM and NICE data ",Sys.Date()))
push(repo, credentials = git.auth)

# Data municipalities per day

rivm.municipalities.perday <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv",sep=";")
filename.municipality.perday <- paste0("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_",Sys.Date(),".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities.perday, file=filename.municipality.perday,row.names = F)

## Push data municipalities per day to repo
add(repo, path = "*")
commit(repo, all = T, paste0("Data municipalities per day ",Sys.Date()))
push(repo, credentials = git.auth)

## Workflows for databases

source("workflow/dashboards/cases_ggd_agegroups.R")
source("workflow/dashboards/date_statistics_mutations.R")
source("workflow/parse_age-data.R")
source("workflow/dashboards/heatmap-age-week.R")
