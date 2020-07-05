require(taskscheduleR)
require(dplyr)
require(rjson)
require(rtweet)
get_token()

setwd("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19")


rivm.data <- read.csv("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv", sep=";") ## Read in data with all cases until today
filename <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_total_cumulative/COVID-19_casus_landelijk_",Sys.Date(),".csv")

write.csv(rivm.data, file=filename) ## Write file with all cases until today

rivm.data$Week <- substr(rivm.data$Week_of_death, 5, 6) ## Add week of death

rivm.death <- rivm.data %>%
  dplyr::filter(Deceased == "Yes") ## Extract deaths data only

rivm.hospital <- rivm.data %>%
  dplyr::filter(Hospital_admission == "Yes") ## Extract hospital data only

rivm.dailydata <- data.frame(Sys.Date(),nrow(rivm.data),nrow(rivm.hospital),nrow(rivm.death)) ## Calculate totals for cases, hospitalizations, deaths
names(rivm.dailydata) <- c("date","cases","hospitalization","deaths")

filename.daily <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_data/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data

write.csv(rivm.dailydata, file = filename.daily) ## Write file with daily data

rivm.daily_aggregate <- read.csv("rivm.daily_aggregate.csv") ## Read in aggregate data
rivm.daily_aggregate <- rivm.daily_aggregate[,-1] ## Remove identifier column

rivm.daily_aggregate <- rbind(rivm.dailydata, rivm.daily_aggregate) ## Bind data today with aggregate data per day
write.csv(rivm.daily_aggregate, file = "C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/rivm.daily_aggregate.csv") ## Write file with aggregate data per day

rivm.daily_aggregate <- read.csv("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/rivm.daily_aggregate.csv")

cases.yesterday <- head(diff(rivm.daily_aggregate$cases),n=1)*-1 ## Calculate new cases
hospital.yesterday <- head(diff(rivm.daily_aggregate$hospitalization),n=1)*-1 ## Calculate new hospitalizations
deaths.yesterday <- head(diff(rivm.daily_aggregate$deaths),n=1)*-1 ## Calculate new deaths

cases.patient <- ifelse(cases.yesterday == 1, "patient","patienten")
hospital.patient <- ifelse(hospital.yesterday == 1, "patient","patienten")
deaths.patient <- ifelse(deaths.yesterday == 1, "patient","patienten")


## Build tweets
tweet <- paste0("RIVM publiceert de dagelijkse update niet meer, dus dan doen we het zelf: 

",cases.yesterday," ",cases.patient," positief getest 
(totaal: ",nrow(rivm.data),") 
",
hospital.yesterday," ",hospital.patient," opgenomen 
(totaal: ",nrow(rivm.hospital),") 
",
deaths.yesterday," ",deaths.patient," overleden 
(totaal: ",nrow(rivm.death),")")

tweet

post_tweet(status = tweet) ## Post tweet

my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
reply_id <- my_timeline$status_id[4] ## Status ID for reply
post_tweet("Voor een veel uitgebreidere update verwijs ik graag naar de dagelijkse updates van @edwinveldhuizen die dit ook per gemeente doet.",
           in_reply_to_status_id = reply_id) ## Post reply


post_tweet(status = "Het aantal positief geteste per provincie sinds 1 juni. Zuid-Holland stijgt nog steeds door terwijl de rest afvlakt/afgevlakt is.",media="C:/Users/s379011/surfdrive/projects/CoronaWatchNL/plots/province_count_time.png",in_reply_to_status_id = reply_id)

post_tweet(status ="Laatste voor vandaag: het aantal besmettingen per leeftijdsgroep vanaf 1 juni. Meeste besmettingen in groep 0-59 en laatste drie weken minimaal aantal in groep 60+.",media="C:/Users/s379011/surfdrive/projects/CoronaWatchNL/plots/overview_plot_leeftijd.png",
           in_reply_to_status_id = reply_id)


## Data for municipalities

rivm.municipalities <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv", sep=";")
filename.municipality <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_municipality_cumulative/rivm_municipality_",Sys.Date(),".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities, file=filename.municipality)

gemeentes <- c("Rotterdam","'s-Gravenhage")

gemeente <- filter(rivm.municipalities,Municipality_name %in% gemeentes)

