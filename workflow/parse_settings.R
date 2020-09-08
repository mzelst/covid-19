require(tabulizer)

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
