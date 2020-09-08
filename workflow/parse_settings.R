require(tabulizer)
require(tidyverse)

weeknumber <- isoweek(Sys.Date())-1

locate_areas("https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200901_1353_1.pdf",
           pages=c(16))

dat <- extract_tables("https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200901_1353_1.pdf",
                      output = "data.frame",
                      pages = c(17),
                      area = list(
                        c(120, 55, 563, 555)),
                      guess=FALSE)
df <- do.call(rbind,dat)

colnames(df) <- c("Settings","Aantal_6juli","perc_6juli","Aantal_week","perc_week")
write.csv(df,file = "data-dashboards/settings.csv", row.names = F)

settings <- extract_tables("https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200901_1353_1.pdf",
                           output = "data.frame",
                           pages = c(16),
                           area = list(
                             c(220, 55, 340, 544)),
                           guess=FALSE)
settings <- do.call(rbind,settings)
colnames(settings) <- c("Related_cases_present","Aantal_6juli","perc_6juli","Aantal_week","perc_week")
write.csv(settings,file = "data-dashboards/settings-total.csv", row.names = F)

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

area.table <- locate_areas("https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200901_1353_1.pdf",
                           pages=c(22))

ggd_tests <- extract_tables("https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200901_1353_1.pdf",
                             output = "data.frame",
                             pages = c(22),
                             area = area.table,
                             guess=FALSE, )
ggd_tests <- do.call(rbind,ggd_tests)

ggd_tests <- ggd_tests[c(2:(nrow(ggd_tests)-1)),]
ggd_tests[nrow(ggd_tests),1] <- weeknumber

## Contactinventarisatie

area.table <- locate_areas("https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200901_1353_1.pdf",
             pages=c(20))


contactinv <- extract_tables("https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200901_1353_1.pdf",
                      output = "data.frame",
                      pages = c(20),
                      area = area.table,
                      guess=FALSE)
contactinv <- do.call(rbind,contactinv)

colnames(contactinv) <- c("Week","Nieuwe_meldingen","Aantal_BCO","Perc_BCO","Aantal_contact","Perc_contact")
