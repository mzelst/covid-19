require(tabulizer)
require(tidyverse)

weeknumber <- isoweek(Sys.Date())-1

report <- "https://www.rivm.nl/sites/default/files/2020-10/COVID-19_WebSite_rapport_wekelijks_20201020_1141.pdf"


## Totaal - settings

area.table.settings.total <- locate_areas(report,
             pages=c(23))

settings <- extract_tables(report,
                           output = "data.frame",
                           pages = c(23),
                           area = area.table.settings.total,
                           guess=FALSE)
settings <- do.call(rbind,settings)
colnames(settings) <- c("Related_cases_present","Aantal_6juli","perc_6juli","Aantal_week","perc_week")
write.csv(settings,file = "data-dashboards/settings-total.csv", row.names = F)

## Alle settings
area.table.settings.specific <- locate_areas(report,
             pages=c(24))

dat <- extract_tables(report,
                      output = "data.frame",
                      pages = c(24),
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
                           pages=c(29))

ggd_tests <- extract_tables(report,
                             output = "data.frame",
                             pages = c(29),
                             area = area.table.ggdpos.rate,
                             guess=FALSE, )
ggd_tests <- do.call(rbind,ggd_tests)

ggd_tests <- ggd_tests[c(2:(nrow(ggd_tests)-1)),]
ggd_tests[nrow(ggd_tests),1] <- weeknumber
ggd_tests$Week <- ggd_tests$Weeknummer

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
             pages=c(27))


contactinv <- extract_tables(report,
                      output = "data.frame",
                      pages = c(27),
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

write.csv(all.data, file = "data-dashboards/report_data.csv")


## Plots
str(all.data)

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
