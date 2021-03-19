require(stringr)
require(tidyr)
require(cbsodataR)
require(reshape2)
require(lubridate)

weeknumber <- isoweek(ymd(Sys.Date()))-2
week.readfile <- isoweek(Sys.Date())-1

table_mortality <- cbs_get_data("70895ned", Perioden = has_substring(c("2001","2002","2003","2004","2005","2006","2013","2014","2015","2016","2017","2018","2019","2020","2021")), Geslacht = has_substring("1100"))
table_mortality$Year <- substr(table_mortality$Perioden, 1, 4)

table_mortality$Week <- str_sub(table_mortality$Perioden, start = -2)

table_mortality$LeeftijdOp31December <- factor(table_mortality$LeeftijdOp31December, levels = c(10000, 41700, 53950, 21700),
                                               labels = c("Totaal","0 tot 65", "65 tot 80", "80+"))

bevolking <- cbs_get_data("37296ned", Perioden = has_substring(c("2001JJ00","2002JJ00","2003JJ00","2004JJ00","2005JJ00","2006JJ00","2013JJ00","2014JJ00","2015JJ00", "2016JJ00","2017JJ00","2018JJ00","2019JJ00","2020JJ00")))
bevolking <- bevolking[,c("Perioden","TotaleBevolking_1", "JongerDan20Jaar_10","k_20Tot40Jaar_11","k_40Tot65Jaar_12","k_65Tot80Jaar_13","k_80JaarOfOuder_14")]
colnames(bevolking) <- c("Jaar","Totaal","Jonger20","20tot40","40tot65","65tot80","80ouder")

bevolking$jonger65 <- bevolking$Jonger20 + bevolking$`20tot40` + bevolking$`40tot65`
bevolking <- bevolking[,c("Jaar","Totaal","jonger65","65tot80","80ouder")]


## Get 2021 data
bevolking2021 <- cbs_get_data("83482NED", Perioden = has_substring(c("2021MM01")),
                                           Generatie = has_substring(c("T001040")),
                                           Migratieachtergrond = has_substring(c("T001040")),
                                           Geslacht = has_substring(c("T001038")), typed = T)

bevolking2021 <- bevolking2021 %>%
  filter(Leeftijd >= 70000 | Leeftijd == 22200)

bevolking2021 <- data.frame(cbind(2021,
                               sum(bevolking2021[1:21,"BevolkingOpDeEersteVanDeMaand_1"]),
                               sum(bevolking2021[1:13,"BevolkingOpDeEersteVanDeMaand_1"]),
                               sum(bevolking2021[14:16,"BevolkingOpDeEersteVanDeMaand_1"]),
                               sum(bevolking2021[17:21,"BevolkingOpDeEersteVanDeMaand_1"])))
colnames(bevolking2021) <- c("Jaar","Totaal","jonger65","65tot80","80ouder")

bevolking <- rbind(bevolking, bevolking2021)


colnames(bevolking) <- c("Year","Totaal","0 tot 65","65 tot 80","80+")
bevolking$Year <- substr(bevolking$Year, 1, 4)

bevolking.long <- gather(bevolking,"LeeftijdOp31December","Bevolking",2:5)

#bevolking$Bevolking <- as.numeric(bevolking$Bevolking)


## Select weeks
table_mortality <- subset(table_mortality, Week > 00 & Week != 53  & Week != '01')
table_mortality <- table_mortality[!table_mortality$Week == '00',]

#& Week < weeknumber+2

mortality_full <- merge(table_mortality, bevolking.long, by=c("Year","LeeftijdOp31December"), all.x=TRUE)

# Reframe bevolking 2021 for merge
bevolking2021 <- t(bevolking2021[1,2:5])
bevolking2021 <- data.frame(bevolking2021,LeeftijdOp31December=c("Totaal","0 tot 65","65 tot 80","80+"), row.names = NULL)
colnames(bevolking2021) <- c("Bevolking2021","LeeftijdOp31December")


#bevolking2021.test <- data.frame(Bevolking2020=c(14017269, 2618728, 838696, 17474693),
#                          LeeftijdOp31December=c("0 tot 65","65 tot 80","80+","Totaal"))

mortality_full <- merge(mortality_full, bevolking2021, by=c("LeeftijdOp31December"))
mortality_full$Overledenen_1 <- mortality_full$Overledenen_1/mortality_full$Bevolking*mortality_full$Bevolking2021

mortality_wide <- dcast(mortality_full, LeeftijdOp31December + Week ~ Year, value.var = "Overledenen_1", sum)
mortality_wide$`2021` <- na_if(mortality_wide$`2021`, 0)


mortality_wide$Average20162020 <- rowMeans(mortality_wide[,c("2016","2017","2018","2019","2020")])
mortality_wide$Average20152019 <- rowMeans(mortality_wide[,c("2015","2016","2017","2018","2019")])

mortality_wide$excess_death2021 <- round(mortality_wide$`2021` - mortality_wide$Average20162020,0)
mortality_wide$excess_death2020 <- round(mortality_wide$`2020` - mortality_wide$Average20152019,0)

excess_deaths2020 <- aggregate(excess_death2020 ~ LeeftijdOp31December + Week, data = mortality_wide, FUN = sum)
excess_deaths2021 <- aggregate(excess_death2021 ~ LeeftijdOp31December + Week, data = mortality_wide, FUN = sum)
excess_deaths2020$Year <- 2020
excess_deaths2021$Year <- 2021
colnames(excess_deaths2020) <- c("LeeftijdOp31December","Week","excess_death","Year")
colnames(excess_deaths2021) <- c("LeeftijdOp31December","Week","excess_death","Year")

excess_deaths <- rbind(excess_deaths2020, excess_deaths2021)


excess_deaths_wide <- spread(excess_deaths, key = LeeftijdOp31December, value = excess_death)
excess_deaths_wide$total_deaths_corrected <- excess_deaths_wide$`0 tot 65` + excess_deaths_wide$`65 tot 80` + excess_deaths_wide$`80+`
excess_deaths_wide$Week <- as.numeric(excess_deaths_wide$Week)

#griep <- subset(mortality_wide, Week > 13)
#excess_flu <- aggregate(excess_flu ~ LeeftijdOp31December, data = griep, FUN = sum)

#hittegolf2006 <- subset(mortality_wide, Week > 26 & Week < 31)
#excess_heatwave <- aggregate(excess_heatwave ~ LeeftijdOp31December, data = hittegolf2006, FUN = sum)

#alleen_ondersterfte <- subset(mortality_wide, Week > 19)
#less_deaths <- aggregate(excess_death ~ LeeftijdOp31December + Week, data = alleen_ondersterfte, FUN = sum)

#age_corrected_less <- round((sum(less_deaths$excess_death) - less_deaths[less_deaths$LeeftijdOp31December == "Totaal","excess_death"]),0)

#week.benchmark <- round(mortality_wide[which(mortality_wide$Week == weeknumber & mortality_wide$LeeftijdOp31December == "Totaal"),"Average20152019"],0)

#week.2020 <- round(mortality_wide[which(mortality_wide$Week == weeknumber & mortality_wide$LeeftijdOp31December == "Totaal"),"2020"],0)

## Calculate official death numbers

nl_dt <- read.csv("corrections/deaths_perweek.csv")[,c("Week","weekdeath_today","Year")]
nl_dt <- data.table(nl_dt)
nl_dt <- nl_dt[,c(1,3,2)]
colnames(nl_dt) <- c("Week","Year","covid_deaths")
nl_dt$covid_deaths <- as.numeric(nl_dt$covid_deaths)
nl_dt$Year <- as.numeric(nl_dt$Year)
nl_dt <- nl_dt[c(1:(nrow(nl_dt)-1)),] ## Only use data up to week 30

excess_deaths_wide <- merge(excess_deaths_wide, nl_dt, by=c("Week","Year"), all.x=T)

deaths_2020 <- mortality_wide %>%
  select(LeeftijdOp31December,Week,`2020`) %>%
  spread(LeeftijdOp31December, `2020`) %>%
  add_column(Year = 2020)

deaths_2021 <- mortality_wide %>%
  select(LeeftijdOp31December,Week,`2021`) %>%
  spread(LeeftijdOp31December, `2021`) %>%
  add_column(Year = 2021)

deaths_2020 <- rbind(deaths_2020,deaths_2021)
deaths_2020$Week <- as.numeric(deaths_2020$Week)

deaths_weekly <- merge(deaths_2020, excess_deaths_wide, by = c("Week","Year"))

## Merge excess mortality with DLM

week.now <- week(Sys.Date())-2 ## Which week?

excess_cbsmodel <- read.csv(paste0("workflow/excess_mortality/data/run_week",week.now,".csv"))

df_cbsmodel <- excess_cbsmodel[which(excess_cbsmodel$model=="Dynamisch"),c("week","deaths_week_low",
                                                                           "deaths_week_mid","deaths_week_high","deaths_low_cumsum",
                                                                           "deaths_mid_cumsum","deaths_high_cumsum","year")]

colnames(df_cbsmodel) <- c("Week","DLModel_lowerbound95","DLModel_week_estimate",
                           "DLModel_upperbound95","Oversterfte_DLModel_cumul_low","Oversterfte_DLModel_cumul_mid",
                           "Oversterfte_DLModel_cumul_high","Year")

df_cbsmodel <- df_cbsmodel %>%
  mutate(DLModel_lowerbound95 = round(DLModel_lowerbound95,0)) %>%
  mutate(DLModel_upperbound95 = round(DLModel_upperbound95,0)) %>%
  mutate(DLModel_week_estimate = round(DLModel_week_estimate,0)) %>%
  mutate(Oversterfte_DLModel_cumul_low = round(Oversterfte_DLModel_cumul_low,0)) %>%
  mutate(Oversterfte_DLModel_cumul_mid = round(Oversterfte_DLModel_cumul_mid,0)) %>%
  mutate(Oversterfte_DLModel_cumul_high = round(Oversterfte_DLModel_cumul_high,0))

colnames(deaths_weekly) <- c("Week","Year","Totaal_Overleden","Overleden0_65","Overleden65_80","Overleden80+",
                                  "Oversterfte_Totaal","Oversterfte0_65","Oversterfte65_80","Oversterfte80+",
                                  "Oversterfte_Totaal_Gecorrigeerd","covid_sterfgevallen")

deaths_weekly <- merge(deaths_weekly, df_cbsmodel,by=c("Week","Year"),all.y=T)

## RIVM excess mortality
excess.mort.rivm <- read.csv("data-misc/excess_mortality/excess_mortality_rivm.csv")
colnames(excess.mort.rivm) <- c("Year","Week","start_week","end_week","lower_bound","upper_bound","mortality","excess_mortality_rivm","expected_mortality")
deaths_weekly <- merge(deaths_weekly, excess.mort.rivm[,c("Year","Week","excess_mortality_rivm")],by=c("Week","Year"),all.x=T)

## CBS death statistics
u.cbs <- "https://www.cbs.nl/nl-nl/nieuws/2021/10/bijna-3-2-duizend-mensen-overleden-aan-covid-19-in-november-2020"
webpage.cbs <- read_html(u.cbs)

cbs.death.statistics <- as.data.frame(html_table(webpage.cbs)[[3]])
cbs.death.statistics$Year <- 2020
colnames(cbs.death.statistics) <- c("Week","Mortality_without_covid_CBS","Covid_deaths_CBS_death_statistics","Year")

deaths_weekly <- merge(deaths_weekly, cbs.death.statistics, by = c("Week","Year"), all.x=T)

str(deaths_weekly)

deaths_weekly <- deaths_weekly %>%
  mutate(Totaal_Overleden = round(Totaal_Overleden,0)) %>%
  mutate(Overleden0_65 = round(Overleden0_65,0)) %>%
  mutate(Overleden65_80 = round(Overleden65_80,0)) %>%
  mutate(`Overleden80+` = round(`Overleden80+`,0))


# Arrange and write file
deaths_weekly <- arrange(deaths_weekly, Year, Week)
write.csv(deaths_weekly, file = "data-misc/excess_mortality/excess_mortality.csv", row.names = F)

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Excess mortality analyses - Week ", week.readfile))
push(repo, credentials = git.auth)
