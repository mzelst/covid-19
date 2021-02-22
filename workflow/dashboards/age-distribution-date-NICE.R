## Age distribution - Clinical

temp = list.files(path = "data-nice/age/Clinical_Beds",pattern="*.csv", full.names = T)
myfiles = lapply(temp, read.csv)

nice_by_day <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})
str(nice_by_day)

nice_by_day <- nice_by_day[,c("Leeftijd","Klinisch_aanwezig","Datum")]

perc <- nice_by_day %>% 
  group_by(Datum) %>% mutate(value = round((Klinisch_aanwezig/sum(Klinisch_aanwezig))*100,2))

nice_by_day_perc <- cbind(nice_by_day[,c("Leeftijd","Datum")],as.numeric(perc$value))
colnames(nice_by_day_perc) <- c("Leeftijd","Datum","Klinisch_aanwezig_Percentage")

Klinisch_aanwezig <- nice_by_day %>%
  spread(Datum,value = Klinisch_aanwezig)

Klinisch_aanwezig_Percentage <- nice_by_day_perc %>%
  spread(Datum,value = Klinisch_aanwezig_Percentage)

write.csv(Klinisch_aanwezig, file = "data-nice/age/leeftijdsverdeling_datum_Klinisch.csv", row.names = F)
write.csv(Klinisch_aanwezig_Percentage, file = "data-nice/age/leeftijdsverdeling_datum_Klinisch_Percentage.csv", row.names = F)



## Age distribution - IC

temp = list.files(path = "data-nice/age/IC",pattern="*.csv", full.names = T)
myfiles = lapply(temp, read.csv)

nice_by_day <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

nice_by_day <- nice_by_day[,c("Leeftijd","IC_aanwezig","Datum")]

perc <- nice_by_day %>% 
  group_by(Datum) %>% mutate(value = round((IC_aanwezig/sum(IC_aanwezig))*100,2))

nice_by_day_perc <- cbind(nice_by_day[,c("Leeftijd","Datum")],as.numeric(perc$value))
colnames(nice_by_day_perc) <- c("Leeftijd","Datum","IC_Aanwezig_Percentage")

IC_Aanwezig <- nice_by_day %>%
  spread(Datum,value = IC_aanwezig)

IC_Aanwezig_Percentage <- nice_by_day_perc %>%
  spread(Datum,value = IC_Aanwezig_Percentage)

write.csv(IC_Aanwezig, file = "data-nice/age/leeftijdsverdeling_datum_IC.csv", row.names = F)
write.csv(IC_Aanwezig_Percentage, file = "data-nice/age/leeftijdsverdeling_datum_IC_Percentage.csv", row.names = F)

add(repo, path = "*")
commit(repo, all = T, paste0("Update NICE age-distribution in hospital ",Sys.Date()))
push(repo, credentials = git.auth)

rm(list=ls())
