require(rvest)
require(RSelenium)
require(tidyverse)
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

## Data for municipalities

rivm.municipalities <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv", sep=";")
filename.municipality <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_municipality_cumulative/rivm_municipality_",Sys.Date(),".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities, file=filename.municipality)


## Pull cumulative IC data
json_file <- "https://www.stichting-nice.nl/covid-19/public/intake-cumulative"
json_data <- fromJSON(file=json_file, simplify = TRUE)
ic.cumulative <- data.frame(matrix(unlist(json_data), nrow=length(json_data), byrow=T))
ic.cumulative$X2 <- as.numeric(ic.cumulative$X2)


cases.yesterday <- head(diff(rivm.daily_aggregate$cases),n=1)*-1 ## Calculate new cases
hospital.yesterday <- head(diff(rivm.daily_aggregate$hospitalization),n=1)*-1 ## Calculate new hospitalizations
deaths.yesterday <- head(diff(rivm.daily_aggregate$deaths),n=1)*-1 ## Calculate new deaths
ic.yesterday <- tail(diff(ic.cumulative$X2),n=1) ## Calculate new IC intakes

cases.patient <- ifelse(cases.yesterday == 1, "patiënt","patiënten")
hospital.patient <- ifelse(hospital.yesterday == 1, "patiënt","patiënten")
deaths.patient <- ifelse(deaths.yesterday == 1, "patiënt","patiënten")
ic.patient <- ifelse(ic.yesterday == 1, "patiënt","patiënten")


## Build tweets
tweet <- paste0("Dagelijkse corona-cijfers update: 

",cases.yesterday," ",cases.patient," positief getest 
(totaal: ",nrow(rivm.data),") 
",
hospital.yesterday," ",hospital.patient," opgenomen 
(totaal: ",nrow(rivm.hospital),") 
",
ic.yesterday," ",ic.patient," opgenomen op de IC 
(totaal: ",tail(ic.cumulative$X2,n=1),") 
",
deaths.yesterday," ",deaths.patient," overleden 
(totaal: ",nrow(rivm.death),")")

tweet

post_tweet(status = tweet) ## Post tweet

my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
reply_id <- my_timeline$status_id[1] ## Status ID for reply
post_tweet("Voor een veel uitgebreidere update verwijs ik graag naar de dagelijkse updates van @edwinveldhuizen die dit ook per gemeente doet.",
           in_reply_to_status_id = reply_id) ## Post reply

post_tweet(status = "Het RIVM publiceert nu de wekelijkse updates op dinsdag (vandaag dus). Zie voor de update over afgelopen week de site van het @RIVM: https://www.rivm.nl/coronavirus-covid-19/actueel",in_reply_to_status_id = reply_id)



## SCRAPE DATA FROM Stichting NICE website ##

rD <- rsDriver(browser="firefox") #start browser #
remDr <- rD$client   # start client #

u.ic <- "https://www.stichting-nice.nl/covid-19-op-de-ic.jsp" #website for IC data
remDr$navigate(u.ic) # Load website

doc <- remDr$getPageSource()[[1]] %>% read_html() # pull rendered source

ic.today.text <- html_node(doc, xpath="/html/body/div[1]/div[2]/div[2]/p[1]") %>% html_text() #download #patients on IC today
ics.used.text <- html_node(doc, xpath="/html/body/div[1]/div[2]/div[2]/p[2]/text()") %>% html_text() #download #ICs used today
ic.cumulative.text <- html_node(doc, xpath="/html/body/div[1]/div[2]/div[2]/p[3]/text()") %>% html_text() #download cumulative #patients on IC
ic.deaths.text <- html_node(doc, xpath="/html/body/div[1]/div[2]/div[2]/p[4]/text()") %>% html_text() # Cumulative #patients that died in IC

u.hospital <- "https://www.stichting-nice.nl/covid-19-op-de-zkh.jsp" #website for hospital departments (not IC) data
remDr$navigate(u.hospital) #Load website
doc <- remDr$getPageSource()[[1]] %>% read_html()   # pull rendered source

hospital.today.text <- html_node(doc, xpath="/html/body/div[1]/div[2]/div[2]/p[1]") %>% html_text() #patients in hospital (not IC) today
hospitals.cumulative.text <- html_node(doc, xpath="/html/body/div[1]/div[2]/div[2]/p[2]/text()") %>% html_text() #cumulative patients in hospital (not IC)
hospital.deaths.text <- html_node(doc, xpath="/html/body/div[1]/div[2]/div[2]/p[3]/text()") %>% html_text() # cumulative deaths in hospital (not IC)

# Transform all downloaded strings into numbers 
ic.today <- as.numeric(sub('.*(\\d{2}).*', '\\1', ic.today.text))
ics.used <- as.numeric(sub('.*(\\d{2}).*', '\\1', ics.used.text))
ic.cumulative <- as.numeric(sub('.*(\\d{4}).*', '\\1', ic.cumulative.text))
hospital.today <- as.numeric(sub('.*(\\d{3}).*', '\\1', hospital.today.text))
hospital.deaths <- as.numeric(sub('.*(\\d{4}).*', '\\1', hospital.deaths.text))



