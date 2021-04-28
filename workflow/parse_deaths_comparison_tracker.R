## Deaths NICE ##
temp = tail(list.files(path = "data-nice/exit/Clinical_Beds",pattern="*.csv", full.names = T),1)
deaths_clinic <- fread(temp)[,c("date","Overleden")]
temp = tail(list.files(path = "data-nice/exit/IC",pattern="*.csv", full.names = T),1)
deaths_IC <- fread(temp)[,c("date","Overleden")]

deaths_nice <- merge(deaths_clinic, deaths_IC, by = "date")
deaths_nice$deaths_nice <- deaths_nice$Overleden.x+deaths_nice$Overleden.y
deaths_nice <- deaths_nice %>%
  mutate(deaths_nice = c(0,diff(deaths_nice))) %>%
  mutate(Week = isoweek(date)) %>%
  mutate(Year = isoyear(date))

deaths_nice <- aggregate(deaths_nice ~ Week + Year, data = deaths_nice, FUN = sum)

## Deaths nursing homes
temp = tail(list.files(path = "data-rivm/nursing-homes-datasets/",pattern="*.csv.gz", full.names = T),1)
nursing.homes <- fread(temp)

nursing.homes$Date_of_statistic_reported <- as.Date(nursing.homes$Date_of_statistic_reported)
nursing.homes.deaths.wide <- aggregate(Total_deceased_reported ~ Date_of_statistic_reported, data = nursing.homes, FUN = sum)
nursing.homes.deaths.wide <- nursing.homes.deaths.wide %>%
  mutate(Week = isoweek(Date_of_statistic_reported)) %>%
  mutate(Year = isoyear(Date_of_statistic_reported))

week_deaths_nursery <- aggregate(Total_deceased_reported ~ Week + Year, data = nursing.homes.deaths.wide, FUN = sum)

deaths_total <- merge(deaths_nice,week_deaths_nursery, by = c("Week","Year"))
df_deaths_rivm <- read.csv("corrections/deaths_perweek.csv")[,c("Week","Year","weekdeath_today")]
deaths_total <- merge(deaths_total, df_deaths_rivm, by = c("Week","Year"), all.x=T)
colnames(deaths_total) <- c("Week","Year","deaths_nice","deaths_nursing","deaths_rivm")
setorder(deaths_total, Year,Week)

deaths_total$deaths_nonnursing_RIVM <- deaths_total$deaths_rivm-deaths_total$deaths_nursing
deaths_total$deaths_nice_nursing <- deaths_total$deaths_nice + deaths_total$deaths_nursing
## Deaths excess DLM / CBS

excess_dlm <- read.csv("data-misc/excess_mortality/excess_mortality.csv")[,c("Week","Year","DLModel_week_estimate")]
colnames(excess_dlm) <- c("Week","Year","total_covid_mortality")
deaths_total <- merge(deaths_total,excess_dlm,by=c("Week","Year"), all.x=T)
setorder(deaths_total, Year, Week)

deaths_total$excess_rivm_nice <- deaths_total$deaths_nice-deaths_total$deaths_nonnursing_RIVM
deaths_total$week_year <- ifelse(deaths_total$Week<10,
                                 paste0(deaths_total$Year,"-",0,deaths_total$Week),
                                 paste0(deaths_total$Year,"-",deaths_total$Week))


write.csv(deaths_total, file = "corrections/death_week_comparisons.csv", row.names = F)

add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Update death comparison tracker"))
push(repo, credentials = git.auth)

rm(deaths_clinic, deaths_IC,deaths_nice,deaths_total,df_deaths_rivm,excess_dlm,nursing.homes,nursing.homes.deaths.wide,
   week_deaths_nursery)
