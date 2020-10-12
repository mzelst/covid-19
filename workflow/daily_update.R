pull(repo)

# Generate Banner
source("workflow/generate_banner.R")

# Parse RIVM, NICE and corrections data
source("workflow/parse_nice-data.R")
source("workflow/parse_rivm-data.R")
#source("workflow/parse_municipalities.R")
source("workflow/parse_corrections.R")

# get tokens
source("workflow/twitter/token_mzelst.R")
source("workflow/twitter/token_edwinveldhuizen.R")

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
tweet.main <- paste0("#COVID19NL statistieken t.o.v. gisteren: 

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



posted_tweet <- post_tweet (
  tweet.main,
  token = token.mzelst,
  media = (paste0("banners/",Sys.Date(),".png"))
) ## Post tweet
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.main.id <- posted_tweet$id_str
tweet.last_id <- tweet.main.id

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

tweet.nice <- paste0("#COVID19NL statistieken t.o.v. gisteren (data NICE): 

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

# Tweet for report ####
posted_tweet <- post_tweet (
  tweet.nice,
  token = token.mzelst,
  media = "plots/plot_daily.png",
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

########
# report
########
tweet.report = "Ik heb een start gemaakt met een dagelijks epidemiologisch rapport (work in progress). Hierin vindt u kaarten en tabellen met gegevens per leeftijdsgroep, provincie, en GGD: https://github.com/mzelst/covid-19/raw/master/reports/daily_report.pdf"
posted_tweet <- post_tweet (
  tweet.report,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

########
# Municipality tweet - cases
########
source("workflow/generate_municipality_images.R")
Sys.setlocale("LC_TIME", "nl_NL")

tweet.municipality.date <- Sys.Date() %>%
  format('%d %b') %>%
  str_to_title()  %>%
  str_replace( '^0', '')

tweet.municipality.colors <- read.csv("data/municipality-totals-color.csv", fileEncoding = "UTF-8")
tweet.municipality.cases <- "Geconstateerde besmettingen per gemeente %s

%s %d / 355 gemeentes

%s %d / 355 gemeentes

Zie de eerste afbeelding voor een uitgebreide uitleg

Let op: vrijwel alle gemeentes minder dan +30 passen niet meer op de eerste afbeelding

[%s]"

tweet.municipality.cases <- sprintf(tweet.municipality.cases,
  intToUtf8(0x1F447),
  intToUtf8(0x1F6D1),
  tweet.municipality.colors$d0[[4]],
  intToUtf8(0x1F7E3),
  tweet.municipality.colors$d0[[5]],
  tweet.municipality.date
)

Encoding(tweet.municipality.cases) <- "UTF-8"

posted_tweet <- post_tweet (
  tweet.municipality.cases,
  token = token.edwinveldhuizen,
  media = c("plots/list-cases-head.png", "plots/list-cases-all-part1.png", "plots/list-cases-all-part2.png", "plots/list-cases-all-part3.png"),
  in_reply_to_status_id = tweet.main.id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str


########
# Municipality tweet - hospital admissions
########
tweet.municipality.hosp <- "Positief geteste patiënten per gemeente die zijn opgenomen met specifiek COVID-19 als reden v. opname

[%s]"

tweet.municipality.hosp <- sprintf(tweet.municipality.hosp,
  tweet.municipality.date
)

Encoding(tweet.municipality.hosp) <- "UTF-8"

posted_tweet <- post_tweet (
  tweet.municipality.hosp,
  token = token.edwinveldhuizen,
  media = c("plots/list-hosp-head.png", "plots/list-hosp-all-part1.png", "plots/list-hosp-all-part2.png", "plots/list-hosp-all-part3.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

########
# Municipality tweet - deaths
########

tweet.municipality.deaths <- "Patiënten per gemeente die positief getest zijn op COVID-19 en helaas zijn overleden

[%s]

Onze condoleance en veel sterkte aan alle nabestaanden. %s"

tweet.municipality.deaths <- sprintf(tweet.municipality.deaths,
  tweet.municipality.date,
  intToUtf8(0x1F339)
)

Encoding(tweet.municipality.deaths) <- "UTF-8"

posted_tweet <- post_tweet (
  tweet.municipality.deaths,
  token = token.edwinveldhuizen,
  media = c("plots/list-deaths-head.png", "plots/list-deaths-all-part1.png", "plots/list-deaths-all-part2.png", "plots/list-deaths-all-part3.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

rm(tweet.municipality.cases, tweet.municipality.hosp, tweet.municipality.deaths, tweet.municipality.colors, tweet.municipality.date)


#post_tweet("Vergeet ook niet de tweets hieronder van @edwinveldhuizen te checken voor de regionale verschillen en trends.",
  #         in_reply_to_status_id = get_reply_id()) ## Post reply

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
