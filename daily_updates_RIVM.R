require(cowplot)
require(tidyverse)
require(rjson)
require(rtweet)
require(data.table)
dir.create("plots")

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

rivm.dailydata <- data.frame(as.Date(Sys.Date()),nrow(rivm.data),nrow(rivm.hospital),nrow(rivm.death)) ## Calculate totals for cases, hospitalizations, deaths
names(rivm.dailydata) <- c("date","cases","hospitalization","deaths")

filename.daily <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_data/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data

write.csv(rivm.dailydata, file = filename.daily) ## Write file with daily data

rivm.daily_aggregate <- read.csv("rivm.daily_aggregate.csv") ## Read in aggregate data
rivm.daily_aggregate <- rivm.daily_aggregate[,-1] ## Remove identifier column

rivm.daily_aggregate <- rbind(rivm.dailydata, rivm.daily_aggregate) ## Bind data today with aggregate data per day
rivm.daily_aggregate <- rivm.daily_aggregate[order(rivm.daily_aggregate$date),]
rivm.daily_aggregate$date <- as.Date(rivm.daily_aggregate$date)

write.csv(rivm.daily_aggregate, file = "C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/rivm.daily_aggregate.csv") ## Write file with aggregate data per day

## Data for municipalities

rivm.municipalities <- read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv", sep=";")
filename.municipality <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_municipality_cumulative/rivm_municipality_",Sys.Date(),".csv") ## Filename for daily data municipalities

write.csv(rivm.municipalities, file=filename.municipality)

## Stichting NICE data

# IC patients died, discharged, discharged to other department (cumulative)
ics.used <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/ic-count",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

ic.died_survivors <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/died-and-survivors-cumulative", simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

ic.death_survive <- as.data.frame(t(ic.died_survivors[c(2,4,6),]))

ic.death_survive$ic_deaths <- unlist(ic.death_survive$V1)
ic.death_survive$ic_discharge <- unlist(ic.death_survive$V2)
ic.death_survive$ic_discharge_inhosp <- unlist(ic.death_survive$V3)
ic.death_survive <- ic.death_survive[,c(4:6)]

# New patients at IC 
ic_intake <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

ic_intake <- as.data.frame(t(ic_intake[c(2,4),]))

ic_intake$ic_intake_proven <- unlist(ic_intake$V1)
ic_intake$ic_intake_suspected <- unlist(ic_intake$V2)
ic_intake <- ic_intake[,c(3:4)]

# IC patients currently
ic_current <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# IC patients cumulative
ic.cumulative <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Number of patients currently in hospital (non-IC) 
zkh_current <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Intake per day of patients in hospital (non-IC) with suspected and/or proven covid-19
json_zkh_df <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

zkh_new <- as.data.frame(t(json_zkh_df[c(1,2,4),]))

zkh_new$date <- unlist(zkh_new$V1)
zkh_new$new_hosp_proven <- unlist(zkh_new$V2)
zkh_new$new_hosp_suspected <- unlist(zkh_new$V3)
zkh_new <- zkh_new[,c(4:6)]


# Merge all data
df <- data.frame(zkh_new,ic_intake,ic_current$value,ics.used$value,ic.cumulative$value,zkh_current$value,ic.death_survive)
names(df) <- c("date","Hospital_Intake_Proven","Hospital_Intake_Suspected","IC_Intake_Proven","IC_Intake_Suspected","IC_Current","ICs_Used","IC_Cumulative","Hospital_Currently","IC_Deaths_Cumulative","IC_Discharge_Cumulative","IC_Discharge_InHospital")
df$Hospital_Intake <- df$Hospital_Intake_Proven + df$Hospital_Intake_Suspected
df$IC_Intake <- df$IC_Intake_Proven + df$IC_Intake_Suspected

# Cumulative sums for suspected cases in hospital
df <- df %>% mutate(Hosp_Intake_Suspec_Cumul = cumsum(Hospital_Intake_Suspected))
df <- df %>% mutate(IC_Intake_Suspected_Cumul = cumsum(IC_Intake_Suspected))
df$date <- as.Date(df$date)

write.csv(df, "C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/daily_nice_data/Cumulative_NICE.csv") ## Write file with all NICE data until today

all.data <- merge(rivm.daily_aggregate,df, by = "date") # Merge RIVM with NICE data
all.data <- all.data %>%
  mutate(positivetests = c(0,diff(cases))) # Calculate number of positive tests per day

all.data$positive_7daverage <- round(frollmean(all.data[,"positivetests"],7),0)

write.csv(all.data, file = "C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/all_data.csv")

## Parse corrections per day
source("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/corrections/parse_corrections.R")


all.data <- read.csv("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/all_data.csv")
all.data$date <- as.Date(all.data$date)

all.data <- merge(all.data,corrections.perday,by="date",all.x=TRUE)

filter.date <- Sys.Date()-28 # Set filter date for last 4 weeks

write.csv(all.data, file = "C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/all_data.csv") # Write dataframe
setwd("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/")
# Plot for positive tests per day
cases <- all.data %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=positivetests)) + 
  geom_line(aes(y = positivetests, color = "Toename besmettingen per dag (incl. correcties)"), lwd=1.2) +
  geom_line(aes(y = positive_7daverage, color = "Voortschrijdend gemiddelde (7 dagen)"), lwd=1.2) +
  geom_line(aes(y = new.infection, color = "Nieuw gemelde besmettingen per dag"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Besmettingen per dag",
       color = "Legend") +
  ggtitle("Meldingen van geconstateerde besmettingen") +
  ggsave("plots/positieve_tests_per_dag.png",width=15, height = 4)

# Plot for #patients in hospital per day

aanwezig <- all.data %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=Hospital_Currently)) + 
  geom_line(aes(y = Hospital_Currently, color = "Aanwezig op verpleegafdeling"), lwd=1.2) +
  geom_line(aes(y = IC_Current, color = "Aanwezig op IC"), lwd=1.2) +
  ylim(0,250) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Totaal aanwezig",
       color = "Legend") +
  ggtitle("Aanwezig op de IC vs. verpleegafdeling") +
  ggsave("plots/overview_aanwezig_zkh.png", width = 15, height=4)

# Plot for #patients intake per day

opnames <- all.data %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=Hospital_Intake_Proven)) + 
  geom_line(aes(y = Hospital_Intake_Proven, color = "Opname op verpleegafdeling"), lwd=1.2) +
  geom_line(aes(y = IC_Intake_Proven, color = "Opname op IC"), lwd=1.2) +
  ylim(0,15) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Opnames per dag",
       color = "Legend") +
  ggtitle("Opnames op de IC vs. verpleegafdeling") +
  ggsave("plots/overview_opnames_zkh.png", width = 15, height=4)

# Merge plots into grid
plot.daily <- plot_grid( aanwezig + theme(legend.position="bottom"),
                   opnames + theme(legend.position="bottom"),
                   cases + theme(legend.position = "bottom", legend.direction = "vertical"),
                   align = 'hv',
                   nrow = 2,
                   hjust = -1
)

# Save grid plot for daily use
save_plot("plots/plot_daily.png", plot.daily, base_asp = 1.1, base_height = 7, base_width = 10)


## Build tweets
tweet <- paste0("#COVID19NL statistieken t.o.v. gisteren: 

Positief getest: ",last(all.data$new.infection),"
Totaal: ",last(all.data$cases)," (+",last(all.data$net.infection)," ivm ",last(all.data$corrections.cases)," corr.)

Opgenomen: ",last(all.data$new.hospitals),"
Totaal: ",last(all.data$hospitalization),ifelse(last(all.data$net.hospitals)>=0," (+"," (-"),abs(last(all.data$net.hospitals))," ivm ",last(all.data$corrections.hospitals)," corr.)

Opgenomen op IC: ",last(all.data$IC_Intake_Proven),"
Totaal: ",tail(all.data$IC_Cumulative,n=1)," 

Overleden: ",last(all.data$new.deaths),"
Totaal: ",last(all.data$deaths),ifelse(last(all.data$net.deaths)>=0," (+"," (-"),abs(last(all.data$net.deaths))," ivm ",last(all.data$corrections.deaths)," corr.)")

tweet

today.date <- paste0("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/banners/",Sys.Date(),".png")

post_tweet (status = tweet,media = today.date) ## Post tweet

# Tweet for hospital numbers

tweet2 <- paste0("Update met betrekking tot ziekenhuis-gegevens (data NICE): 

Patiënten verpleegafdeling 
Bevestigd: ",tail(all.data$Hospital_Intake_Proven,n=1),". Verdacht: ",tail(all.data$Hospital_Intake_Suspected, n=1),".

Patiënten IC
Bevestigd: ",tail(all.data$IC_Intake_Proven,n=1),". Verdacht: ",tail(all.data$IC_Intake_Suspected,n=1),".

Grafisch per dag: Het aantal bevestigde aanwezige patienten in het ziekenhuis, opnames, en aantal besmettingen.")

setwd("C:/Users/s379011/surfdrive/projects/2020covid-19/covid-19/")
# Tweet for graph
my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
reply_id <- my_timeline$status_id[1] ## Status ID for reply
post_tweet(tweet2, media = "plots/plot_daily.png",
           in_reply_to_status_id = reply_id) ## Post reply

my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
reply_id <- my_timeline$status_id[1] ## Status ID for reply
post_tweet("Vergeet ook niet de tweets hieronder van @edwinveldhuizen te checken voor de regionale verschillen en trends. Wellicht zijn deze nog belangrijker dan de landelijke cijfers.",
           in_reply_to_status_id = reply_id) ## Post reply


my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
reply_id <- my_timeline$status_id[1] ## Status ID for reply
post_tweet(status = "Het RIVM publiceert nu de wekelijkse updates op dinsdag (vandaag dus). Zie voor de update over afgelopen week de site van het @RIVM: https://www.rivm.nl/sites/default/files/2020-07/COVID-19_WebSite_rapport_wekelijks_20200714_1040.pdf",in_reply_to_status_id = reply_id)
