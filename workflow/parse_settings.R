require(tabulizer)
require(tidyverse)

weeknumber <- isoweek(Sys.Date())-1

report <- "https://www.rivm.nl/sites/default/files/2020-11/COVID-19_WebSite_rapport_wekelijks_20201117_1237.pdf"


## Totaal - settings

area.table.settings.total <- locate_areas(report,
             pages=c(21))

settings <- extract_tables(report,
                           output = "data.frame",
                           pages = c(21),
                           area = area.table.settings.total,
                           guess=FALSE)
settings <- do.call(rbind,settings)
colnames(settings) <- c("Related_cases_present","Aantal_6juli","perc_6juli","Aantal_week","perc_week")
write.csv(settings,file = "data-dashboards/settings-total.csv", row.names = F)

## Alle settings
area.table.settings.specific <- locate_areas(report,
             pages=c(22))

dat <- extract_tables(report,
                      output = "data.frame",
                      pages = c(22),
                      area = area.table.settings.specific,
                      guess=FALSE)
df <- do.call(rbind,dat)
df <- df[1:29,]

colnames(df) <- c("Settings","Aantal_6juli","perc_6juli","Aantal_week","perc_week")
write.csv(df,file = "data-dashboards/settings.csv", row.names = F)



infections <- read.csv("corrections/corrections_perday.csv")
infections$Week <- isoweek(infections$date)

infections.perweek <- aggregate(net.infection ~ Week, data = infections, FUN=sum)
infections.lastweek <- infections.perweek[(nrow(infections.perweek)-1),"net.infection"]
infections.twoweeksago <- infections.perweek[(nrow(infections.perweek)-2),"net.infection"]

hospitals.perweek <- aggregate(net.hospitals ~ Week, data = infections, FUN=sum)
hospitals.lastweek <- hospitals.perweek[(nrow(hospitals.perweek)-1),"net.hospitals"]
hospitals.twoweeksago <- hospitals.perweek[(nrow(hospitals.perweek)-2),"net.hospitals"]

deaths.perweek <- aggregate(net.deaths ~ Week, data = infections, FUN=sum)
deaths.lastweek <- deaths.perweek[(nrow(deaths.perweek)-1),"net.deaths"]
deaths.twoweeksago <- deaths.perweek[(nrow(deaths.perweek)-2),"net.deaths"]

sum.settings <- sum(df$Aantal_week)
number.settings <- settings[2,4]

perc.known <- number.settings/infections.lastweek
perc.home <- df[1,4]/number.settings
perc.family <- df[2,4]/number.settings
perc.friends <- df[4,4]/number.settings
perc.parties <- df[10,4]/number.settings

settings.perpatient <- number.settings/sum.settings

perc.private.known <- round((perc.home+perc.family)*perc.known*settings.perpatient*100,1)
perc.priv_extend.known <- round((perc.home+perc.family+perc.friends+perc.parties)*perc.known*settings.perpatient*100,1)

## GGD Positive rate

area.table.ggdpos.rate <- locate_areas(report,
                           pages=c(28))

ggd_tests <- extract_tables(report,
                             output = "data.frame",
                             pages = c(28),
                             area = area.table.ggdpos.rate,
                             guess=FALSE, )
ggd_tests <- do.call(rbind,ggd_tests)
ggd_tests <- ggd_tests[c(2:(nrow(ggd_tests)-1)),]
ggd_tests$Week <- c(23:weeknumber)

## Tests door labs

area.table.testlabs <- locate_areas(report,
                           pages=c(43))


tests.labs <- extract_tables(report,
                             output = "data.frame",
                             pages = c(43),
                             area = area.table.testlabs,
                             guess=FALSE)
tests.labs <- do.call(rbind,tests.labs)

colnames(tests.labs) <- c("Datum","Aantal_labs","Tests","Aantal_positief","Perc_positief")

tests.labs <- tests.labs[c(2:(nrow(tests.labs))),]
tests.labs$Week <- c(11:weeknumber)


## Contactinventarisatie

area.table.contacts <- locate_areas(report,
             pages=c(26))


contactinv <- extract_tables(report,
                      output = "data.frame",
                      pages = c(26),
                      area = area.table.contacts,
                      guess=FALSE)
contactinv <- do.call(rbind,contactinv)
contactinv <- contactinv[c(2:(nrow(contactinv))),]
colnames(contactinv) <- c("Week","Nieuwe_meldingen","Aantal_BCO","Perc_BCO","Aantal_contact","Perc_contact")

write.csv(contactinv, file = "data-dashboards/settings.csv")

## Merge data

weekly_datalist <- list(tests.labs, ggd_tests, contactinv)

all.data <- Reduce(
  function(x, y, ...) merge(x, y, by="Week",all.x = TRUE, ...),
  weekly_datalist
)

colnames(all.data) <- c("Week","Datum-weken","Aantal_Labs","Tests_Labs","Positief_Labs","Percentage_Labs",
                        "Weeknummer","Tests_GGD","Positief_GGD","Percentage_GGD","Meldingen_BCO","Positief_via_BCO",
                        "Percentage_via_BCO","Contactinventarisaties","Perc_inven_uitgevoerd")

all.data$Weeknummer <- c(11:weeknumber)

write.csv(all.data, file = "data-dashboards/report_data.csv")


all.data <- read.csv("data-dashboards/report_data.csv")

perc_pos <- all.data %>%
  filter(Week > 22) %>%
  ggplot(aes(x=Week, y=Percentage_GGD)) + 
  geom_line(aes(y = Percentage_GGD, color = "Percentage Positief (GGD)"), lwd=1.2) +
  geom_text(aes(label=Percentage_GGD),hjust=1, vjust=-1) +
  geom_point(size = 2,alpha = 0.6) +
  geom_line(aes(y = Percentage_Labs, color = "Percentage Positief (Labs)"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 2), limits = c(0, NA)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Week",
       y = "Percentage",
       color = "Legend") +
  ggtitle("Percentage Tests Positief") +
  ggsave("plots/percentage_positief.png",width=12, height = 10)


perc_inventarisatie <- all.data %>%
  filter(Week > 26) %>%
  ggplot(aes(x=Week, y=Perc_inven_uitgevoerd)) + 
  geom_line(aes(y = Perc_inven_uitgevoerd, color = "Percentage Contactinventarisatie (%)"), lwd=1.2) +
  geom_text(aes(label=Perc_inven_uitgevoerd),hjust=1, vjust=-1) +
  geom_point(size = 2,alpha = 0.6) +
  scale_y_continuous(expand = c(0, 10), limits = c(0, NA)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Week",
       y = "Percentage",
       color = "Legend") +
  ggtitle("Percentage Contactinventarisatie Afgerond") +
  ggsave("plots/percentage_inventarisatie.png",width=12, height = 10)




## Healthcare employees


dat.healthcare <- read.csv("data-misc/healthcare_employees.csv")
dat.healthcare <- dat.healthcare %>%
  mutate(cases_new_healthcare = c(0,diff(cases_cumulative_healthcare))) %>%
  mutate(cases_new_education = c(0,diff(cases_cumulative_education))) %>%
  mutate(hospitalized_healthcare_new = c(0,diff(hospitalized_healthcare))) %>%
  mutate(hospitalized_education_new = c(0,diff(hospitalized_education))) %>%
  mutate(deaths_healthcare_new = c(0,diff(deaths_healthcare))) %>%
  mutate(deaths_education_new = c(0,diff(deaths_education)))

dat.healthcare$date <- as.Date(dat.healthcare$date, format = "%d-%m-%Y")

dat.healthcare %>%
  ggplot(aes(x = date, y = cases_new)) +
  geom_line(aes(y = cases_new_healthcare, color = "Besmettingen per week - Zorgmedewerkers"), lwd=1.5) +
  geom_line(aes(y = cases_new_education, color = "Besmettingen per week - Onderwijsmedewerkers"), lwd=1.5) +
  scale_y_continuous(expand = c(0, 500), limits = c(0, NA)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        plot.title.position = "plot",
        plot.caption = element_text(size = 6),
        legend.position = "bottom",
        axis.ticks.length = unit(0.5, "cm"),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid")) +
  labs(x = "Datum",
       y = "Aantal",
       color = "Legenda") +
  ggtitle("Besmettingen per week") +
  ggsave("plots/healthcare_emp_cases.png",
         width = 16, height = 10, units = "cm", device='png')

dat.healthcare %>%
  ggplot(aes(x = date, y = hospital_new)) +
  geom_line(aes(y = hospitalized_healthcare_new, color = "Zorgmedewerkers"), lwd=1.5) +
  geom_line(aes(y = hospitalized_education_new, color = "Onderwijsmedewerkers"), lwd=1.5) +
  scale_y_continuous(expand = c(0, 1), limits = c(0, NA)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        plot.title.position = "plot",
        plot.caption = element_text(size = 6),
        legend.position = "none",
        axis.ticks.length = unit(0.5, "cm"),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid")) +
  labs(x = "Datum",
       y = "Aantal",
       color = "Legenda") +
  ggtitle("Opnames per week") +
  ggsave("plots/healthcare_emp_hospital.png",
         width = 16, height = 10, units = "cm", device='png')

dat.healthcare %>%
  ggplot(aes(x = date, y = deaths_new)) +
  geom_line(aes(y = deaths_healthcare_new, color = "Zorgmedewerkers"), lwd=1.5) +
  geom_line(aes(y = deaths_education_new, color = "Onderwijsmedewerkers"), lwd=1.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        plot.title.position = "plot",
        plot.caption = element_text(size = 6),
        legend.position = "none",
        axis.ticks.length = unit(0.5, "cm"),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid")) +
  labs(x = "Datum",
       y = "Aantal",
       color = "Legenda") +
  ggtitle("Sterfte per week") +
  ggsave("plots/healthcare_emp_deaths.png",
         width = 16, height = 10, units = "cm", device='png')



