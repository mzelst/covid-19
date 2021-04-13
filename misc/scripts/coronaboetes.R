dat <- read.csv("https://pastebin.com/raw/Tc14UP46")
dat$Municipality_name <- dat$Gemeente

mun <- read.csv("misc/municipalities-population.csv")
dat$Municipality_name <- recode(dat$Municipality_name,"Den Haag" = "'s-Gravenhage",
                                "Den Bosch" = "'s-Hertogenbosch",
                                "Noardeast-FryslÃ¢n" = "Noardeast-Fryslân",
                                "SÃºdwest-FryslÃ¢n" = "Súdwest-Fryslân")

df <- merge(dat,mun, by = "Municipality_name",all.x=T)

df <- df %>%
  mutate(Mondkapje_publiekeruimte = round(Mondkapjesplicht.publieke.ruimte/population*100,2)) %>%
  mutate(Noodverordening = round(Noodverordening/population*100,2)) %>%
  mutate(Verbod_groepsvorming = round(Verbod.groepsvorming/population*100,2)) %>%
  mutate(Verbod_alcohol = round(Verbod.alcohol/population*100,2)) %>%
  mutate(Mondkapje_OV = round(Mondkapjesplicht.in.personenvervoer/population*100,2)) %>%
  mutate(Avondklok = round(Avondklok/population*100,2)) %>%
  mutate(Totaal_percentage = round(Totaal/population*100,2))

total.data <- df %>%
  select(Municipality_name:Totaal,Municipality_code:Totaal_percentage)

write.csv(total.data, file = "misc/coronaboetes.csv")

cases <- read.csv("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_2021-03-31.csv.gz")
cases.cumulative <- aggregate(Total_reported ~ Municipality_name, data=cases,FUN=sum)
hosp.cumulative <- aggregate(Hospital_admission ~ Municipality_name, data=cases,FUN=sum)
deaths.cumulative <- aggregate(Deceased ~ Municipality_name, data=cases,FUN=sum)

total.data <- merge(total.data,cases.cumulative,by="Municipality_name")
total.data <- merge(total.data,hosp.cumulative,by="Municipality_name")
total.data <- merge(total.data,deaths.cumulative,by="Municipality_name")

total.data$perc_infected <- total.data$Total_reported/total.data$population*100
total.data$perc_hospital <- total.data$Hospital_admission/total.data$population*100
total.data$perc_deceased <- total.data$Deceased/total.data$population*100

cor(total.data$Totaal_percentage,total.data$perc_infected)
cor(total.data$Totaal_percentage,total.data$perc_hospital)
cor(total.data$Totaal_percentage,total.data$perc_deceased)


hospital.nice <- read.csv("data-rivm/municipal-hospital-datasets/rivm_hospital_2021-03-31.csv.gz")
hospital.nice.cumulative <- aggregate(Hospital_admission ~ Municipality_name, data=cases,FUN=sum)
total.data <- merge(total.data,hospital.nice.cumulative,by="Municipality_name")
total.data$perc_hospital_nice <- total.data$Hospital_admission.y/total.data$population*100
cor(total.data$Totaal_percentage,total.data$perc_hospital_nice)
