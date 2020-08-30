require(tidyverse)
require(git2r)


temp = list.files(path = "data-rivm/casus-datasets/",pattern="*.csv", full.names = T) ## Pull names of all available datafiles
dat <- read.csv(last(temp), )
dat$value <- 1

dat$Municipal_health_service <- recode(dat$Municipal_health_service, "GGD FryslÃ¢n" = "GGD Fryslân",
                                       "GGD Zuid Limburg" = "GGD Zuid-Limburg",
                                       "GGD Brabant Zuid-Oost " = "GGD Brabant-Zuidoost",
                                       "GGD Hollands Midden" = "GGD Hollands-Midden",
                                       "GGD Limburg Noord" = "GGD Limburg-Noord",
                                       "GGD Zaanstreek-Waterland" = "GGD Zaanstreek/Waterland",
                                       "GGD West Brabant" = "GGD West-Brabant",
                                       "GGD Rotterdam Rijnmond" = "GGD Rotterdam-Rijnmond",
                                       "GGD regio Utrecht" = "GGD Regio Utrecht",
                                       "GGD Gelderland-Midden" = "Veiligheids- en Gezondheidsregio Gelderland-Midden",
                                       "GGD Hollands Noorden" = "GGD Hollands-Noorden",
                                       "GGD Noord en Oost Gelderland" = "GGD Noord- en Oost-Gelderland")

ggd_data_long <- aggregate(dat$value, by = list(Leeftijd = dat$Agegroup, statnaam = dat$Municipal_health_service, Datum = dat$Date_statistics), FUN = sum)

ggd.desc.data <- read.csv("misc/ggds-population.csv")[,c("statnaam","ggd_code")]

ggd_data <- merge(ggd_data_long, ggd.desc.data, by = "statnaam")
colnames(ggd_data) <- c("ggd_naam","age_group","date","cases","ggd_code")
ggd_data <- ggd_data %>%
  select(age_group, date, cases, ggd_code) %>%
  arrange(date) %>%
  relocate(date, .before = age_group)

write.csv(ggd_data, file = "data-dashboards/cases_ggd_agegroups.csv", row.names = F)

add(repo, path = "data-dashboards/cases_ggd_agegroups.csv")
commit(repo, all = T, paste0("Update cases per ggd per agegroup ",Sys.Date()))
push(repo, credentials = git.auth)

