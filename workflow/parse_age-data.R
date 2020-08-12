dat <- read.csv("data-rivm/casus-datasets/COVID-19_casus_landelijk_2020-08-11.csv") %>%
  dplyr::filter(Agegroup != "<50" & Agegroup != "Unknown")
dat$week <- strftime(dat$Date_statistics, format = "%V")
dat$value <- 1

dat_tidy <- aggregate(dat$value, by = list(Leeftijd = dat$Agegroup, Week = dat$week), FUN = sum)
dat_tidy$Week <- as.numeric(dat_tidy$Week)
colnames(dat_tidy) <- c("Leeftijd","Week","Besmettingen")

dat_besmettingen_abs <- dat_tidy %>%
  spread(Week,value = Besmettingen)



perc <- dat_tidy %>% 
  group_by(Week) %>% mutate(value = round((Besmettingen/sum(Besmettingen))*100,2))

dat_tidy <- cbind(dat_tidy[,c("Leeftijd","Week")],as.numeric(perc$value))
colnames(dat_tidy) <- c("Leeftijd","Week","Besmettingen")

dat_besmettingen_perc <- dat_tidy %>%
  spread(Week,value = Besmettingen)

dat_leeftijd <- rbind(dat_besmettingen_abs,dat_besmettingen_perc)

write.csv(dat_leeftijd, file = "misc/age-week.csv")


