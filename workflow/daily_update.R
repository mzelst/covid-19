pull(repo)

# Generate Banner
source("workflow/generate_banner.R")

#rivm.by_day <- read.csv("data/rivm_by_day.csv")

# Verify RIVM data has been downloaded, otherwise stop script.
#condition <- Sys.Date()!=as.Date(last(rivm.by_day$date))

#if (condition) {stop("The value is TRUE, so the script must end here")    
#} else {

# Parse RIVM, NICE and corrections data
source("workflow/parse_nice-data.R")
source("workflow/parse_lcps-data.R")
source("workflow/parse_rivm-data.R")
source("workflow/parse_nursing-homes.R")
source("workflow/parse_tests.R")
source("workflow/parse_corrections.R")

## Set locale
Sys.setlocale("LC_TIME", "nl_NL")

## Merge RIVM, NICE and corrections data

rivm.by_day <- read.csv("data/rivm_by_day.csv")  
nice.by_day <- read.csv("data-nice/nice-today.csv")
lcps.by_day <- read.csv("data/lcps_by_day.csv")
corr.by_day <- read.csv("corrections/corrections_perday.csv")
nursery.by_day <- read.csv("data/nursery_by_day.csv")
testrate.by_day <- read.csv("data-dashboards/percentage-positive-daily-national.csv")[,c("values.tested_total","values.infected","values.infected_percentage","date","pos.rate.3d.avg")]
#vaccines.by_day <- read.csv("data/vaccines_by_day.csv") , vaccines.by_day

daily_datalist <- list(rivm.by_day,nice.by_day,lcps.by_day,corr.by_day,nursery.by_day, testrate.by_day)

all.data <- Reduce(
  function(x, y, ...) merge(x, y, by="date",all.x = TRUE, ...),
  daily_datalist
)

all.data$date <- as.Date(all.data$date)
all.data <- all.data[order(all.data$date),]

write.csv(all.data, file = "data/all_data.csv",row.names = F)

all.data <- read.csv("data/all_data.csv")
#vaccines.by_day <- read.csv("data/vaccines_by_day.csv")

# get tokens
source("workflow/twitter/token_mzelst.R")
#source("workflow/twitter/token_edwinveldhuizen.R")

LCPS_klinisch_two_days <- last(all.data$Kliniek_Bedden,2)
LCPS_Verpleeg_Huidig_Toename <- LCPS_klinisch_two_days[2] - LCPS_klinisch_two_days[1]
LCPS_IC_two_days <- last(all.data$IC_Bedden_COVID,2)
LCPS_IC_Huidig_Toename <- LCPS_IC_two_days[2] - LCPS_IC_two_days[1]
 
sign.hosp.lcps <- paste0(ifelse(LCPS_Verpleeg_Huidig_Toename>=0," (+"," ("))
sign.ic.lcps <- paste0(ifelse(LCPS_IC_Huidig_Toename>=0," (+"," ("))

Kliniek_Nieuwe_Opnames <- ifelse(is.na(last(all.data$Kliniek_Nieuwe_Opnames_COVID)),"Onbekend",last(all.data$Kliniek_Nieuwe_Opnames_COVID))
Kliniek_Aanwezig <- ifelse(is.na(last(all.data$Kliniek_Bedden)),"Onbekend",paste0(format(last(all.data$Kliniek_Bedden),decimal.mark = ",",big.mark =".",big.interval = 3),sign.hosp.lcps,LCPS_Verpleeg_Huidig_Toename))
IC_Nieuwe_Opnames <- ifelse(is.na(last(all.data$IC_Nieuwe_Opnames_COVID)),"Onbekend",last(all.data$IC_Nieuwe_Opnames_COVID))
IC_Aanwezig <- ifelse(is.na(last(all.data$IC_Bedden_COVID)),"Onbekend",paste0(last(all.data$IC_Bedden_COVID),sign.ic.lcps,LCPS_IC_Huidig_Toename))

#vaccins.geprikt <- format(last(vaccines.by_day$vaccines_administered_ggd+vaccines.by_day$vaccines_administered_estimated_hospital),decimal.mark = ",",big.mark =".",big.interval = 3)

## Build tweets
tweet.main <- paste0("#COVID19NL

Positief getest: ",format(last(all.data$new.infection),decimal.mark = ",",big.mark =".",big.interval = 3),"
Totaal: ",format(last(all.data$cases),decimal.mark = ",",big.mark =".",big.interval = 3)," (+",format(last(all.data$net.infection),decimal.mark = ",",big.mark =".",big.interval = 3)," ivm ",last(all.data$corrections.cases)," corr.)

Perc. positief ",format(as.Date(Sys.Date()-4), "%d %b")," - ",format(as.Date(Sys.Date()-2), "%d %b"),": ",format(all.data[nrow(all.data)-2,"pos.rate.3d.avg"],decimal.mark = ",",big.mark =".",big.interval = 3),"%

Opgenomen: ",Kliniek_Nieuwe_Opnames,"
Huidig: ",Kliniek_Aanwezig,")

Opgenomen op IC: ",IC_Nieuwe_Opnames,"
Huidig: ",IC_Aanwezig,")

Overleden: ",last(all.data$new.deaths),"
Totaal: ",format(last(all.data$deaths),decimal.mark = ",",big.mark =".",big.interval = 3),"")

tweet.main

posted_tweet <- post_tweet (
  tweet.main,
  token = token.mzelst,
  media = (paste0("banners/",Sys.Date(),".png"))
) ## Post tweet
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.main.id <- posted_tweet$id_str
tweet.last_id <- tweet.main.id

##### Generate municipality images
source("workflow/parse_nice-municipalities-data.R")
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

%s %d / 352 gemeentes

%s %d / 352 gemeentes

%s %d / 352 gemeentes

%s %d / 352 gemeentes

Zie de eerste afbeelding voor een uitgebreide legenda

[%s] #COVID19NL

%s"

tweet.municipality.cases <- sprintf(tweet.municipality.cases,
  intToUtf8(0x1F447),
  intToUtf8(0x1F7E7),
  tweet.municipality.colors$d0[[3]],
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
  token = token.mzelst,
  media = c("plots/list-cases-head.png", "plots/list-cases-all-part1.png", "plots/list-cases-all-part2.png", "plots/list-cases-all-part3.png"),
  in_reply_to_status_id = tweet.main.id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str
rm(tweet.municipality.cases, tweet.municipality.colors, posted_tweet)

########
# Municipality tweet - hospital admissions
########
tweet.municipality.hosp <- "Opgenomen patiënten met positieve test per (woon) gemeente

(bron: Stichting NICE / RIVM)

[%s] #COVID19NL"

tweet.municipality.hosp <- sprintf(tweet.municipality.hosp,
  tweet.municipality.date
)

posted_tweet <- post_tweet (
  tweet.municipality.hosp,
  token = token.mzelst,
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

tweet.municipality.deaths <- "Positief geteste patiënten die helaas zijn overleden per (woon) gemeente

(bron: GGD / RIVM)

[%s] #COVID19NL

Onze condoleance en veel sterkte aan alle nabestaanden. %s"

tweet.municipality.deaths <- sprintf(tweet.municipality.deaths,
  tweet.municipality.date,
  intToUtf8(0x1F339)
)

Encoding(tweet.municipality.deaths) <- "UTF-8"

posted_tweet <- post_tweet (
  tweet.municipality.deaths,
  token = token.mzelst,
  media = c("plots/list-deaths-head.png", "plots/list-deaths-all-part1.png", "plots/list-deaths-all-part2.png", "plots/list-deaths-all-part3.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

rm(tweet.municipality.deaths, tweet.municipality.date, posted_tweet)

# Verify whether NICE update ran correctly

dat.today = as.data.frame(lapply(tail(list.files(path = "data-nice/data-nice-json/",pattern="*.csv", full.names = T),1),read.csv))
nice.date <- as.Date(last(dat.today$date))
today.date <- Sys.Date()

if (nice.date == Sys.Date()){
  ## Workflows for plots
  source("plot_scripts/daily_plots.R")
  #source("plot_scripts/daily_maps_plots.R")
  
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
  
  sign.hosp.nice <- paste0(ifelse(Verpleeg_Huidig_Toename>=0," (+"," ("))
  sign.ic.nice <- paste0(ifelse(IC_Huidig_Toename>=0," (+"," ("))
  
  # Tweet for hospital numbers - Data NICE ####
  
  tweet.nice <- paste0("#COVID19NL statistieken t.o.v. gisteren (data NICE): 

Patiënten verpleegafdeling 
Bevestigd: ",Verpleeg_Opname_Bevestigd,"
Verdacht: ",Verpleeg_Opname_Verdacht,"
Huidig: ",last(dat.today$Hospital_Currently),sign.hosp.nice,Verpleeg_Huidig_Toename,")
Totaal: ",last(dat.today$Hospital_Cumulative),"

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
              "plots/percentage_positief_per_dag.png",
              "plots/overview_aanwezig_zkh.png",
              "plots/overview_opnames_zkh.png"),
    in_reply_to_status_id = tweet.main.id,
    auto_populate_reply_metadata = TRUE
  )
  
} else {
  print("NICE update failed")
}

########
# Tweet - nursery homes
########
all.data <- read.csv("data/all_data.csv")

source("plot_scripts/nursery_homes.R")
new.locations.nursery <- all.data[nrow(all.data),"total.current.locations.nursery"] - all.data[nrow(all.data)-1,"total.current.locations.nursery"]

tweet.nurseryhomes <- paste0("#Verpleeghuis statistieken t.o.v. gisteren: 

Positief getest: ",last(all.data$infections.today.nursery),"
Totaal: ",last(all.data$infections.total.nursery),"

Overleden: ",last(all.data$deaths.today.nursery),"
Totaal: ",last(all.data$deaths.total.nursery),"

Nieuwe locaties met besmettingen: ",new.locations.nursery,"
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

##### Produce daily report ####

##### Download case file
rivm.data <- fread("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv", sep =";") ## Read in data with all cases until today
filename.raw <- paste0("raw-data-archive/casus-datasets/COVID-19_casus_landelijk_",Sys.Date(),".csv")
fwrite(rivm.data, filename.raw,row.names = F) ## Write file with all cases until today

filename.compressed <- paste0("data-rivm/casus-datasets/COVID-19_casus_landelijk_",Sys.Date(),".csv.gz")
fwrite(rivm.data, file=filename.compressed,row.names = F) ## Write file with all cases until today

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc"); rmarkdown::render('reports/daily_report.Rmd') ## Render daily report
file.copy(from = list.files('reports', pattern="*.pdf",full.names = TRUE), 
          to = paste0("reports/daily_reports/Epidemiologische situatie COVID-19 in Nederland - ",
                      format((Sys.Date()),'%d')," ",format((Sys.Date()),'%B')," 2021.pdf")) ## Save daily file in archive

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

#}

## Workflows for databases
rm(list=ls())
source("workflow/estimate_R.R")
source("workflow/dashboards/cases_ggd_agegroups.R")
source("workflow/dashboards/date_statistics_mutations.R")
source("workflow/parse_age-data.R")
source("workflow/dashboards/rivm-date-corrections.R")
source("workflow/dashboards/heatmap-age-week.R")
source("workflow/dashboards/age-distribution-date-NICE.R")

## Vaccine tweet for history ##
#Vaccins geprikt: ",vaccins.geprikt,"
#Vaccins geprikt (geschat): ",format(last(vaccines.by_day$vaccines_administered_estimated),decimal.mark = ",",big.mark =".",big.interval = 3),"

## Workflow for dashboard scrape 
repeat {
  Sys.sleep(10)
  ## Scrape dashboard date
  dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
  dashboard.date <- as.Date(as.POSIXct(last(dat$vaccine_administered_total$values$date_unix), origin="1970-01-01"))
  today.date <- as.Date(Sys.Date())-1
  if (dashboard.date == today.date){
    Sys.sleep(60)
    source("workflow/parse_vaccines.R")
    git.credentials <- read_lines("git_auth.txt")
    git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])
    
    ##Push to git
    repo <- init()
    add(repo, path = "*")
    commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) update vaccine data"))
    push(repo, credentials = git.auth)
    Sys.time()
    break
  }
}


require(gmailr)
gm_auth_configure(path="workflow/twitter/gmailr.json")

latest_msg <- paste0("Daily Update - [", Sys.Date(),"] - completed")

dat <- read.csv("data/all_data.csv")
IC_message <- ifelse(last(dat$IC_Cumulative)>10000,"IC=10000","IC is nog niet 10000")
my_email_message <- gm_mime() %>%
  gm_to("j.m.vanzelst@uvt.nl") %>%
  gm_from("marinovanzelst@gmail.com") %>%
  gm_subject(latest_msg) %>%
  gm_text_body(IC_message)

gm_auth("marinovanzelst@gmail.com")

gm_send_message(my_email_message)

rm(daily_vaccin_datalist,dat,df.care.institutions,df.doctors,df.ggd,df.hospitals,df.total,
   my_email_message,myfiles,vaccine_data,vaccines_by_day,vaccines_delivery,dashboard.date,filename.daily.vaccins,
   filename.daily.vaccins.delivered,IC_message,last.date,latest_msg,temp,today.date)


