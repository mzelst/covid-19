## Age distribution - Clinical

temp = list.files(path = "data-nice/age/Clinical_Beds",pattern="*.csv", full.names = T)
myfiles = lapply(temp, read.csv)

nice_by_day_clinical <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

nice_by_day_clinical_aanwezig <- nice_by_day_clinical[,c("Leeftijd","Klinisch_aanwezig","Datum")]

perc <- nice_by_day_clinical_aanwezig %>% 
  group_by(Datum) %>% mutate(value = round((Klinisch_aanwezig/sum(Klinisch_aanwezig))*100,2))

nice_by_day_perc <- cbind(nice_by_day_clinical_aanwezig[,c("Leeftijd","Datum")],as.numeric(perc$value))
colnames(nice_by_day_perc) <- c("Leeftijd","Datum","Klinisch_aanwezig_Percentage")

Klinisch_aanwezig <- nice_by_day_clinical_aanwezig %>%
  spread(Datum,value = Klinisch_aanwezig)

Klinisch_aanwezig_Percentage <- nice_by_day_perc %>%
  spread(Datum,value = Klinisch_aanwezig_Percentage)

write.csv(Klinisch_aanwezig, file = "data-nice/age/leeftijdsverdeling_datum_Klinisch.csv", row.names = F)
write.csv(Klinisch_aanwezig_Percentage, file = "data-nice/age/leeftijdsverdeling_datum_Klinisch_Percentage.csv", row.names = F)

## Age distribution - IC

temp = list.files(path = "data-nice/age/IC",pattern="*.csv", full.names = T)
myfiles = lapply(temp, read.csv)

nice_by_day_IC <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

nice_by_day_IC_aanwezig <- nice_by_day_IC[,c("Leeftijd","IC_aanwezig","Datum")]

perc <- nice_by_day_IC_aanwezig %>% 
  group_by(Datum) %>% mutate(value = round((IC_aanwezig/sum(IC_aanwezig))*100,2))

nice_by_day_perc <- cbind(nice_by_day_IC_aanwezig[,c("Leeftijd","Datum")],as.numeric(perc$value))
colnames(nice_by_day_perc) <- c("Leeftijd","Datum","IC_Aanwezig_Percentage")

IC_Aanwezig <- nice_by_day_IC_aanwezig %>%
  spread(Datum,value = IC_aanwezig)

IC_Aanwezig_Percentage <- nice_by_day_perc %>%
  spread(Datum,value = IC_Aanwezig_Percentage)

write.csv(IC_Aanwezig, file = "data-nice/age/leeftijdsverdeling_datum_IC.csv", row.names = F)
write.csv(IC_Aanwezig_Percentage, file = "data-nice/age/leeftijdsverdeling_datum_IC_Percentage.csv", row.names = F)

# Merge clinical and IC data into long format

nice_by_day_clinical <- nice_by_day_clinical[,c("Leeftijd","Totaal","Datum")]
nice_by_day_IC <- nice_by_day_IC[,c("Leeftijd","Totaal","Datum")]

nice_by_day_clinical$Type <- "Klinisch"
nice_by_day_IC$Type <- "IC"

colnames(nice_by_day_clinical) <- c("Leeftijd","Totaal","Datum","Type")
colnames(nice_by_day_IC) <- c("Leeftijd","Totaal","Datum","Type")

nice_by_day <- rbind(nice_by_day_clinical, nice_by_day_IC)
nice_by_day_wide <- spread(nice_by_day, Leeftijd, Totaal)
nice_by_day_wide <- nice_by_day_wide %>% select(Datum:`<20`, `20 - 24`:`85 - 89`,`>90`)
write.csv(nice_by_day_wide, file = "data-nice/age/leeftijdsverdeling_datum_Klinisch_IC_long.csv", row.names = F)

## Plot hospital intake per age group (older groups)

dat <- read.csv("data-nice/age/leeftijdsverdeling_datum_Klinisch_IC_long.csv")
dat <- dat %>%
  filter(Type == "Klinisch")
dat$below70 <- rowSums(dat[,3:13])
dat <- dat[,c(1:2,14:19)]
colnames(dat) <- c("Datum","Type","age70_74","age75_79","age80_84","age85_89","age90","age_below70")

dat <- dat %>%
  mutate(age70_74_intake = c(0,diff(age70_74))) %>%
  mutate(age75_79_intake = c(0,diff(age75_79))) %>%
  mutate(age80_84_intake = c(0,diff(age80_84))) %>%
  mutate(age85_89_intake = c(0,diff(age85_89))) %>%
  mutate(age90_intake = c(0,diff(age90))) %>%
  mutate(age_below70_intake = c(0,diff(age_below70))) %>%
  mutate(Datum = as.Date(Datum))

dat <- dat %>%
  mutate(age70_74_intake = round(frollmean(age70_74_intake,7),0)) %>%
  mutate(age75_79_intake = round(frollmean(age75_79_intake,7),0)) %>%
  mutate(age80_84_intake = round(frollmean(age80_84_intake,7),0)) %>%
  mutate(age85_89_intake = round(frollmean(age85_89_intake,7),0)) %>%
  mutate(age90_intake = round(frollmean(age90_intake,7),0)) %>%
  mutate(age_below70_intake = round(frollmean(age_below70_intake,7),0)) %>%
  mutate(Datum = as.Date(Datum))

dat %>%
  ggplot(aes(x=Datum, y=age70_74_intake, group = 1)) + 
  geom_line(aes(y = age_below70_intake, color = "<70"), lwd=1.2) +
  geom_line(aes(y = age70_74_intake, color = "70-74"), lwd=1.2) +
  geom_line(aes(y = age75_79_intake, color = "75-70"), lwd=1.2) +
  geom_line(aes(y = age80_84_intake, color = "80-84"), lwd=1.2) +
  geom_line(aes(y = age85_89_intake, color = "85-89"), lwd=1.2) +
  geom_line(aes(y = age90_intake, color = "90+"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  #scale_color_manual(values = c("#F58121", "#228AC7", "#f79a4d", "#7ab9dd")) +
  guides(colour = guide_legend(reverse=T)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Totaal aanwezig",
       color = "Legend") +
  ggtitle("Opnames per dag - Leeftijdsgroepen (Kliniek)") + 
  ggsave("plots/leeftijd_opnames_kliniek.png",width=12, height = 10)

## Push to github
add(repo, path = "*")
commit(repo, all = T, paste0("Update NICE age-distribution in hospital ",Sys.Date()))
push(repo, credentials = git.auth)

rm(list=ls())