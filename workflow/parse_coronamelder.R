coronamelder.results <- read.csv("https://raw.githubusercontent.com/minvws/nl-covid19-notification-app-statistics/main/statistics/ggd_weekly_tests_following_notification_CM.csv")
coronamelder.auth <- read.csv("https://raw.githubusercontent.com/minvws/nl-covid19-notification-app-statistics/main/statistics/ggd_positive_test_authorisations.csv")
coronamelder.downloads <- read.csv("https://raw.githubusercontent.com/minvws/nl-covid19-notification-app-statistics/main/statistics/appstore_statistics.csv")

colnames(coronamelder.results) <- c("Year","Week","test.requests","tests","positive","positive.rate",
                                    "asymp.tests","asymp.pos","asymp.pos.rate","symp.tests","symp.pos","symp.pos.rate")
colnames(coronamelder.auth) <- c("date","authorisations","cumulative_authorisations")
colnames(coronamelder.downloads) <- c("date","downloads.appstore","downloads.playstore","downloads.huawei","downloads.daily","downloads.cumulative")

coronamelder.auth$Week <- isoweek(coronamelder.auth$date)
coronamelder.auth$Year <- isoyear(coronamelder.auth$date)

coronamelder.auth <- aggregate(authorisations ~ Week + Year, data = coronamelder.auth, FUN = sum)

coronamelder <- merge(coronamelder.results, coronamelder.auth, by = c("Year","Week"))
coronamelder$test.per.auth <- coronamelder$test.requests/coronamelder$authorisations



coronamelder.pos <- sum(coronamelder.results$Total.Positive)
asymptomatic.pos <- sum(coronamelder.results$Asymptomatic.Positive, na.rm=T)

downloads <- last(coronamelder.downloads$Total.downloads..cumulative.)

ratio.pos.downloads <- coronamelder.pos/downloads
ratio.asymp.downloads <- asymptomatic.pos/downloads
1/ratio.asymp.downloads


dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")

ggd.tests <- dat$tested_ggd_daily$values
ggd.tests$date <- as.Date(as.POSIXct(ggd.tests$date_unix, origin="1970-01-01"))
ggd.tests$Week <- isoweek(ggd.tests$date)
ggd.tests$Year <- isoyear(ggd.tests$date)
ggd.tests.week <- aggregate(infected ~ Week + Year, data = ggd.tests, FUN = sum)

coronamelder <- merge(coronamelder,ggd.tests.week, by = c("Week","Year"))
