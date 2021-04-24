## Age distribution - Clinical - Deaths per day

temp = list.files(path = "data-nice/age/Clinical_Beds",pattern="*.csv", full.names = T)
myfiles = lapply(temp, read.csv)

nice_by_day_clinical <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

nice_by_day_clinical_overleden <- nice_by_day_clinical[,c("Leeftijd","Verlaten_Overleden","Datum")]

nice_by_day_clinical_overleden <- nice_by_day_clinical_overleden[with(nice_by_day_clinical_overleden, order(Leeftijd,Datum)),]

nice_by_day_clinical_overleden <- nice_by_day_clinical_overleden %>%
  group_by(Leeftijd) %>%
  mutate(Overleden_toename = Verlaten_Overleden - lag(Verlaten_Overleden))

nice_by_day_clinical_overleden <- nice_by_day_clinical_overleden[,c("Leeftijd","Overleden_toename","Datum")]

Klinisch_overleden <- nice_by_day_clinical_overleden %>%
  spread(Leeftijd,value = Overleden_toename)

Klinisch_overleden <- Klinisch_overleden %>% select(Datum:`<20`, `20 - 24`:`85 - 89`,`>90`)
Klinisch_overleden$Totaal <- rowSums(Klinisch_overleden[,c(2:17)])

## Age distribution - IC - Deaths per day

temp = list.files(path = "data-nice/age/IC",pattern="*.csv", full.names = T)
myfiles = lapply(temp, read.csv)

nice_by_day_IC <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

nice_by_day_IC_overleden <- nice_by_day_IC[,c("Leeftijd","Verlaten_Overleden","Datum")]

nice_by_day_IC_overleden <- nice_by_day_IC_overleden[with(nice_by_day_IC_overleden, order(Leeftijd,Datum)),]

nice_by_day_IC_overleden <- nice_by_day_IC_overleden %>%
  group_by(Leeftijd) %>%
  mutate(Overleden_toename = Verlaten_Overleden - lag(Verlaten_Overleden))

nice_by_day_IC_overleden <- nice_by_day_IC_overleden[,c("Leeftijd","Overleden_toename","Datum")]

IC_overleden <- nice_by_day_IC_overleden %>%
  spread(Leeftijd,value = Overleden_toename)

IC_overleden <- IC_overleden %>% select(Datum:`<20`, `20 - 24`:`85 - 89`,`>90`)
IC_overleden$Totaal <- rowSums(IC_overleden[,c(2:17)])

Klinisch_overleden$Type <- "Klinisch"
IC_overleden$Type <- "IC"
nice_by_day_overleden <- rbind(Klinisch_overleden, IC_overleden)

df_nice_deaths <- nice_by_day_overleden %>%
  filter(Datum >= "2020-11-04")

df_nice_deaths <- aggregate(Totaal ~ Datum, data = df_nice_deaths, FUN = sum)
df_nice_deaths <- df_nice_deaths %>%
  mutate(deaths_7d = round(frollmean(Totaal,7),0)) %>%
  mutate(Week = isoweek(Datum)) %>%
  mutate(Year = isoyear(Datum))
week_deaths_nice <- aggregate(Totaal ~ Week + Year, data = df_nice_deaths, FUN = sum)


## Nursery homes

temp = tail(list.files(path = "data-rivm/nursing-homes-datasets/",pattern="*.csv.gz", full.names = T),1)
nursing.homes <- fread(temp)

nursing.homes$Date_of_statistic_reported <- as.Date(nursing.homes$Date_of_statistic_reported)
nursing.homes.deaths.wide <- aggregate(Total_deceased_reported ~ Date_of_statistic_reported, data = nursing.homes, FUN = sum)
nursing.homes.deaths.wide <- nursing.homes.deaths.wide %>%
  mutate(Week = isoweek(Date_of_statistic_reported)) %>%
  mutate(Year = isoyear(Date_of_statistic_reported))

week_deaths_nursery <- aggregate(Total_deceased_reported ~ Week + Year, data = nursing.homes.deaths.wide, FUN = sum)

df_deaths_nice_nursing <- merge(week_deaths_nice,week_deaths_nursery, by = c("Week","Year"))
df_deaths_rivm <- read.csv("corrections/deaths_perweek.csv")

deaths_total <- merge(df_deaths_nice_nursing, df_deaths_rivm[,c("Week","Year","weekdeath_today")],by=c("Week","Year"))
setorder(deaths_total, Year, Week)
colnames(deaths_total) <- c("Week","Year","Deaths_hospital","Deaths_nursing","Deaths_RIVM")
deaths_total$Deaths_nonnursing_RIVM <- deaths_total$Deaths_RIVM-deaths_total$Deaths_nursing
deaths_total$excess_rivm_nice <- deaths_total$Deaths_hospital-deaths_total$Deaths_nonnursing_RIVM

write.csv(deaths_total, file = "corrections/death_week_comparisons.csv")