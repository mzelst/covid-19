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
  mutate(Totaal = round(Totaal/population*100,2))

total.data <- df %>%
  select(Municipality_name:Totaal,Municipality_code:Totaal_percentage)

write.csv(total.data, file = "misc/coronaboetes.csv")