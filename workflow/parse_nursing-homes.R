temp = tail(list.files(path = "data-rivm/nursing-homes-datasets/",pattern="*.csv.gz", full.names = T),1)
nursing.homes <- fread(temp)

nursing.homes$Date_of_statistic_reported <- as.Date(nursing.homes$Date_of_statistic_reported)

nursing.homes.cases.wide <- aggregate(Total_cases_reported ~ Date_of_statistic_reported, data = nursing.homes, FUN = sum)
nursing.homes.deaths.wide <- aggregate(Total_deceased_reported ~ Date_of_statistic_reported, data = nursing.homes, FUN = sum)

nursing.homes.wide <- merge(nursing.homes.cases.wide,nursing.homes.deaths.wide, by = c("Date_of_statistic_reported"))

nursing.homes.wide$cases_7daverage_nursinghomes <- round(frollmean(nursing.homes.wide[,"Total_cases_reported"],7),0)
nursing.homes.wide$deceased_7daverage_nursinghomes <- round(frollmean(nursing.homes.wide[,"Total_deceased_reported"],7),0)

date.nursery.homes <- as.Date(Sys.Date())

nursing.homes.wide %>%
  filter(Date_of_statistic_reported > "2020-01-01" & Date_of_statistic_reported < date.nursery.homes) %>%
  ggplot(aes(x = Date_of_statistic_reported, y = cases_7daverage_nursinghomes)) +
  geom_line(aes(y = deceased_7daverage_nursinghomes, color = "Sterfte per dag"), lwd=1.0) +
  geom_line(aes(y = cases_7daverage_nursinghomes, color = "Geconstateerde besmettingen"),lwd=1.0) +
  scale_y_continuous(expand = c(0, 10), limits = c(0, NA)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle=element_text(size=11, hjust=0.5),
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Aantal",
       color = "Legend") +
  ggtitle("Toename positief geteste en overleden verpleeghuis bewoners (7-daags gem.)") +
  ggsave("plots/verpleeghuizen_bewoners.png",width=12, height = 8)

## Counts for nursing homes

temp = tail(list.files(path = "data-rivm/nursing-homes-datasets/",pattern="*.csv.gz", full.names = T),2)
myfiles = lapply(temp, fread)

dat.today <- as.data.frame(myfiles[2])
dat.yesterday <- as.data.frame(myfiles[1])

# Positive tests
nursing.homes.infection <- sum(dat.today$Total_cases_reported) - sum(dat.yesterday$Total_cases_reported)
nursing.homes.deaths <- sum(dat.today$Total_deceased_reported) - sum(dat.yesterday$Total_deceased_reported)

locations <- aggregate(Total_infected_locations_reported ~ Date_of_statistic_reported, data = dat.today, FUN = sum)
locations$Date_of_statistic_reported <- as.Date(locations$Date_of_statistic_reported)

locations.today <- aggregate(Total_new_infected_locations_reported ~ Date_of_statistic_reported, data = dat.today, FUN = sum)
locations.today$Date_of_statistic_reported <- as.Date(locations.today$Date_of_statistic_reported)
locations.yesterday <- aggregate(Total_new_infected_locations_reported ~ Date_of_statistic_reported, data = dat.yesterday, FUN = sum)
locations.yesterday$Date_of_statistic_reported <- as.Date(locations.yesterday$Date_of_statistic_reported)
df.locations.new <- merge(locations.today,locations.yesterday,by="Date_of_statistic_reported", all.x=T)
df.locations.new$diff <- df.locations.new$Total_new_infected_locations_reported.x - df.locations.new$Total_new_infected_locations_reported.y

new_reported_locations <- sum(df.locations.new$diff,na.rm=T)

locations <- merge(locations, locations.today, by = "Date_of_statistic_reported")

## Plot locaties

locations %>%
  filter(Date_of_statistic_reported > "2020-01-01" & Date_of_statistic_reported <= date.nursery.homes) %>%
  ggplot(aes(x = Date_of_statistic_reported, y = Total_infected_locations_reported, group = 1)) +
  geom_line(aes(y = Total_infected_locations_reported, color = "Aantal locaties"), lwd=1.5) +
  scale_y_continuous(expand = c(0, 50), limits = c(0, NA)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle=element_text(size=11, hjust=0.5),
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Aantal",
       color = "Legend") +
  ggtitle("Aantal locaties met geconstateerde besmettingen") +
  ggsave("plots/verpleeghuizen_locaties.png",width=12, height = 8)


nursing.homes.all <- as.data.frame(cbind(nursing.homes.infection,sum(dat.today$Total_cases_reported),nursing.homes.deaths,
                                         sum(dat.today$Total_deceased_reported),new_reported_locations,last(locations$Total_infected_locations_reported)))
nursing.homes.all$date <- as.Date(Sys.Date())

colnames(nursing.homes.all) <- c("infections_today","infections_total","deaths_today","deaths_total","mutations_locations","total_current_locations","date")
write.csv(nursing.homes.all, file = paste0("data-rivm/nursing-homes-per-day/nursery_daily_",Sys.Date(),".csv"),row.names = F)

## Merge all daily files
temp = list.files(path = "data-rivm/nursing-homes-per-day/",pattern="*.csv", full.names = T) ## Fetch all day files
myfiles = lapply(temp, read.csv) ## Load all day files

nursery_by_day <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

nursery_by_day$date <- as.Date(nursery_by_day$date)
nursery_by_day <- nursery_by_day[order(nursery_by_day$date),]
colnames(nursery_by_day) <- c("infections.today.nursery","infections.total.nursery","deaths.today.nursery",
                              "deaths.total.nursery","mutations.locations.nursery","total.current.locations.nursery","date")
write.csv(nursery_by_day, file = "data/nursery_by_day.csv",row.names = F) ## Write file with aggregate data per day

