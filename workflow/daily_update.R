pull(repo)

# Generate Banner
source("workflow/generate_banner.R")

# Parse RIVM, NICE and corrections data
source("workflow/parse_lcps-data.R")
source("workflow/parse_nice-data.R")
source("workflow/parse_rivm-data.R")
source("workflow/parse_nursing-homes.R")
source("workflow/parse_corrections.R")

Sys.setlocale("LC_TIME", "nl_NL")
## Merge RIVM, NICE and corrections data

rivm.by_day <- read.csv("data/rivm_by_day.csv")
nice.by_day <- read.csv("data-nice/nice-today.csv")
lcps.by_day <- read.csv("data/lcps_by_day.csv")
corr.by_day <- read.csv("corrections/corrections_perday.csv")
nursery.by_day <- read.csv("data/nursery_by_day.csv")

daily_datalist <- list(rivm.by_day,nice.by_day,lcps.by_day,corr.by_day,nursery.by_day)

all.data <- Reduce(
  function(x, y, ...) merge(x, y, by="date",all.x = TRUE, ...),
  daily_datalist
)

all.data$date <- as.Date(all.data$date)
all.data <- all.data[order(all.data$date),]

write.csv(all.data, file = "data/all_data.csv",row.names = F)

source("plot_scripts/daily_plots.R")
#source("plot_scripts/daily_maps_plots.R")

all.data <- read.csv("data/all_data.csv")

# get tokens
source("workflow/twitter/token_mzelst.R")
source("workflow/twitter/token_edwinveldhuizen.R")

LCPS_klinisch_two_days <- last(all.data$Kliniek_Bedden,2)
LCPS_Verpleeg_Huidig_Toename <- LCPS_klinisch_two_days[2] - LCPS_klinisch_two_days[1]
LCPS_IC_two_days <- last(all.data$IC_Bedden_COVID,2)
LCPS_IC_Huidig_Toename <- LCPS_IC_two_days[2] - LCPS_IC_two_days[1]
 
sign.hosp.lcps <- paste0(ifelse(LCPS_Verpleeg_Huidig_Toename>=0," (+"," ("))
sign.ic.lcps <- paste0(ifelse(LCPS_IC_Huidig_Toename>=0," (+"," ("))

## Build tweets
tweet.main <- paste0("#COVID19NL statistieken t.o.v. gisteren: 

Positief getest: ",last(all.data$new.infection),"
Totaal: ",last(all.data$cases)," (+",last(all.data$net.infection)," ivm ",last(all.data$corrections.cases)," corr.)

Opgenomen*: ",last(all.data$Kliniek_Nieuwe_Opnames_COVID),"
Huidig*: ",last(all.data$Kliniek_Bedden),sign.hosp.lcps,LCPS_Verpleeg_Huidig_Toename,")

Opgenomen op IC*: ",last(all.data$IC_Nieuwe_Opnames_COVID),"
Huidig*: ",last(all.data$IC_Bedden_COVID),sign.ic.lcps,LCPS_IC_Huidig_Toename,")
* LCPS cijfers - www.lcps.nu

Overleden: ",last(all.data$new.deaths),"
Totaal: ",last(all.data$deaths),"")

tweet.main

posted_tweet <- post_tweet (
  tweet.main,
  token = token.mzelst,
  media = (paste0("banners/",Sys.Date(),".png"))
) ## Post tweet
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.main.id <- posted_tweet$id_str
tweet.last_id <- tweet.main.id

# Retweet for @edwinveldhuizen
post_tweet (token = token.edwinveldhuizen,
  retweet_id = tweet.main.id)

##### Generate municipality images
source("workflow/parse_municipalities.R")
source("workflow/generate_municipality_images.R")
#####

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) update RIVM and NICE data part 1/2"))
push(repo, credentials = git.auth)

########
# Municipality tweet - cases
########

tweet.municipality.date <- Sys.Date() %>%
  format('%d %b') %>%
  str_to_title()  %>%
  str_replace( '^0', '')

tweet.municipality.colors <- read.csv("data/municipality-totals-color.csv", fileEncoding = "UTF-8")
tweet.municipality.cases <- "Geconstateerde besmettingen per gemeente %s

%s %d / 355 gemeentes

%s %d / 355 gemeentes

%s %d / 355 gemeentes

Zie de eerste afbeelding voor een uitgebreide legenda

[%s]

%s"

tweet.municipality.cases <- sprintf(tweet.municipality.cases,
  intToUtf8(0x1F447),
  intToUtf8(0x1F6D1),
  tweet.municipality.colors$d0[[4]],
  intToUtf8(0x1F7E3),
  tweet.municipality.colors$d0[[5]],
  intToUtf8(0x26A1),
  tweet.municipality.colors$d0[[6]],
  tweet.municipality.date,
  'https://raw.githack.com/mzelst/covid-19/master/workflow/daily_municipality.html'
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
rm(tweet.municipality.cases, tweet.municipality.colors, posted_tweet)

post_tweet (
  token.mzelst,
  retweet_id = tweet.last_id)

########
# Municipality tweet - hospital admissions
########
tweet.municipality.hosp <- "Positief geteste patiënten per gemeente die zijn opgenomen met specifiek COVID-19 als reden v. opname

[%s]"

tweet.municipality.hosp <- sprintf(tweet.municipality.hosp,
  tweet.municipality.date
)

posted_tweet <- post_tweet (
  tweet.municipality.hosp,
  token = token.edwinveldhuizen,
  media = c("plots/list-hosp-head.png", "plots/list-hosp-all-part1.png", "plots/list-hosp-all-part2.png", "plots/list-hosp-all-part3.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

rm(tweet.municipality.hosp, posted_tweet)

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

rm(tweet.municipality.deaths, tweet.municipality.date, posted_tweet)

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

# Tweet for NICE ####
posted_tweet <- post_tweet (
  tweet.nice,
  token = token.mzelst,
  media = c("plots/positieve_tests_per_dag.png",
            "plots/overview_aanwezig_zkh.png",
            "plots/overview_opnames_zkh.png"
  ),
  in_reply_to_status_id = tweet.main.id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

########
# Tweet - nursery homes
########

source("plot_scripts/nursery_homes.R")

tweet.nurseryhomes <- paste0("#Verpleeghuis statistieken t.o.v. gisteren: 

Positief getest: ",last(all.data$infections.today.nursery),"
Totaal: ",last(all.data$infections.total.nursery),"

Overleden: ",last(all.data$deaths.today.nursery),"
Totaal: ",last(all.data$deaths.total.nursery),"

Nieuwe locaties met besmettingen: ",last(all.data$mutations.locations.nursery),"
Huidig aantal locaties met besmettingen:* ",last(all.data$total.current.locations.nursery),"
*Locaties waar in de afgelopen 28 dagen minstens één COVID-19 besmetting is gemeld.")


# Tweet for nursery homes ####
posted_tweet <- post_tweet (
  tweet.nurseryhomes,
  token = token.mzelst,
  media = c("plots/nursery_homes_vr_map.png",
            "plots/verpleeghuizen_bewoners.png",
            "plots/verpleeghuizen_locaties.png"
  ),
  in_reply_to_status_id = tweet.main.id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

##### Download case file
rivm.data <- utils::read.csv("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv", sep =";") ## Read in data with all cases until today
filename <- paste0("data-rivm/casus-datasets/COVID-19_casus_landelijk_",Sys.Date(),".csv")
write.csv(rivm.data, file=filename,row.names = F) ## Write file with all cases until today
#####

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc"); rmarkdown::render('reports/daily_report.Rmd') ## Render daily report
file.copy(from = list.files('reports', pattern="*.pdf",full.names = TRUE), 
          to = paste0("reports/daily_reports/Epidemiologische situatie COVID-19 in Nederland - ",
                      format((Sys.Date()),'%d')," ",format((Sys.Date()),'%B'),".pdf")) ## Save daily file in archive

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) update RIVM and NICE data part 2/2"))
push(repo, credentials = git.auth)

########
# report
########
tweet.report = "Wij maken ook een dagelijks epidemiologisch rapport (work in progress). Hierin vindt u kaarten en tabellen met gegevens per leeftijdsgroep, provincie, en GGD: https://github.com/mzelst/covid-19/raw/master/reports/daily_report.pdf"
posted_tweet <- post_tweet (
  tweet.report,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Workflows for databases

source("workflow/dashboards/cases_ggd_agegroups.R")
source("workflow/dashboards/date_statistics_mutations.R")
source("workflow/parse_age-data.R")
source("workflow/dashboards/heatmap-age-week.R")
source("workflow/dashboards/rivm-date-corrections.R")
