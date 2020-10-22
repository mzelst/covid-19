require(stringr)
require(tidyr)
require(cbsodataR)
require(reshape2)
require(lubridate)

weeknumber <- isoweek(ymd(Sys.Date()))-1

table_mortality <- cbs_get_data("70895ned", Perioden = has_substring(c("2001","2002","2003","2004","2005","2006","2013","2014","2015","2016","2017","2018","2019","2020")), Geslacht = has_substring("1100"))
table_mortality$Year <- substr(table_mortality$Perioden, 1, 4)

table_mortality$Week <- str_sub(table_mortality$Perioden, start = -2)

table_mortality$LeeftijdOp31December <- factor(table_mortality$LeeftijdOp31December, levels = c(10000, 41700, 53950, 21700),
                                               labels = c("Totaal","0 tot 65", "65 tot 80", "80+"))

bevolking <- cbs_get_data("37296ned", Perioden = has_substring(c("2001JJ00","2002JJ00","2003JJ00","2004JJ00","2005JJ00","2006JJ00","2013JJ00","2014JJ00","2015JJ00", "2016JJ00","2017JJ00","2018JJ00","2019JJ00")))
bevolking <- bevolking[,c("Perioden","TotaleBevolking_1", "JongerDan20Jaar_10","k_20Tot40Jaar_11","k_40Tot65Jaar_12","k_65Tot80Jaar_13","k_80JaarOfOuder_14")]

colnames(bevolking) <- c("Jaar","Totaal","Jonger20","20tot40","40tot65","65tot80","80ouder")

bevolking$jonger65 <- bevolking$Jonger20 + bevolking$`20tot40` + bevolking$`40tot65`
bevolking <- bevolking[,c("Jaar","Totaal","jonger65","65tot80","80ouder")]
colnames(bevolking) <- c("Year","Totaal","0 tot 65","65 tot 80","80+")
bevolking$Year <- substr(bevolking$Year, 1, 4)

bevolking2020 <- data.frame(c("2020","17414600","14015030", "2570467", "822088"))

bevolking <- rbind(bevolking, c("2020","17414600","14015030", "2570467", "822088"))


bevolking <- gather(bevolking,"LeeftijdOp31December","Bevolking",2:5)

bevolking$Bevolking <- as.numeric(bevolking$Bevolking)


## Select weeks
table_mortality <- subset(table_mortality, Week > 00 & Week < weeknumber+1)
table_mortality <- table_mortality[!table_mortality$Week == '00',]



mortality_full <- merge(table_mortality, bevolking, by=c("Year","LeeftijdOp31December"), all.x=TRUE)

bevolking2020 <- data.frame(Bevolking2020=c(14015030, 2570467, 822088, 17414600),
                            LeeftijdOp31December=c("0 tot 65","65 tot 80","80+","Totaal"))

mortality_full <- merge(mortality_full, bevolking2020, by=c("LeeftijdOp31December"))
mortality_full$Overledenen_1 <- mortality_full$Overledenen_1/mortality_full$Bevolking*mortality_full$Bevolking2020

mortality_wide <- dcast(mortality_full, LeeftijdOp31December + Week ~ Year, value.var = "Overledenen_1", sum)

mortality_wide$Average20152019 <- rowMeans(mortality_wide[,c("2015","2016","2017","2018","2019")])
mortality_wide$Average20132017 <- rowMeans(mortality_wide[,c("2013","2014","2015","2016","2017")])
mortality_wide$Average20012005 <- rowMeans(mortality_wide[,c("2001","2002","2003","2004","2005")])

mortality_wide$excess_death <- round(mortality_wide$`2020` - mortality_wide$Average20152019,0)
mortality_wide$excess_flu <- mortality_wide$`2018` - mortality_wide$Average20132017
mortality_wide$excess_heatwave <- mortality_wide$`2006` - mortality_wide$Average20012005

excess_deaths <- aggregate(excess_death ~ LeeftijdOp31December + Week, data = mortality_wide, FUN = sum)
excess_deaths_wide <- spread(excess_deaths, key = LeeftijdOp31December, value = excess_death)
excess_deaths_wide$total_deaths_corrected <- excess_deaths_wide$`0 tot 65` + excess_deaths_wide$`65 tot 80` + excess_deaths_wide$`80+`

griep <- subset(mortality_wide, Week > 13)
excess_flu <- aggregate(excess_flu ~ LeeftijdOp31December, data = griep, FUN = sum)

hittegolf2006 <- subset(mortality_wide, Week > 26 & Week < 31)
excess_heatwave <- aggregate(excess_heatwave ~ LeeftijdOp31December, data = hittegolf2006, FUN = sum)


#alleen_ondersterfte <- subset(mortality_wide, Week > 19)
#less_deaths <- aggregate(excess_death ~ LeeftijdOp31December + Week, data = alleen_ondersterfte, FUN = sum)

#age_corrected_less <- round((sum(less_deaths$excess_death) - less_deaths[less_deaths$LeeftijdOp31December == "Totaal","excess_death"]),0)

#week.benchmark <- round(mortality_wide[which(mortality_wide$Week == weeknumber & mortality_wide$LeeftijdOp31December == "Totaal"),"Average20152019"],0)

#week.2020 <- round(mortality_wide[which(mortality_wide$Week == weeknumber & mortality_wide$LeeftijdOp31December == "Totaal"),"2020"],0)

## Calculate official death numbers

if(file.exists('/data/covid_deaths_dt.rds')) {
  rivm_dt <- fread(paste0("https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/rivm_NL_covid19_national_by_date/rivm_NL_covid19_national_by_date_",Sys.Date()-3,".csv")
  )[,
    Datum := as.IDate(Datum)
  ][,
    week := week(Datum + 1)
  ]
  saveRDS(rivm_dt, '/data/covid_deaths_dt.rds')
} else {
  rivm_dt <- readRDS('/data/covid_deaths_dt.rds')
}

nl_dt <- rivm_dt[Type == 'Overleden',
                 .(year = 2020, covid_deaths = sum(Aantal)),
                 by = week
]

excess_deaths_wide <- merge(excess_deaths_wide, nl_dt[,c("week","covid_deaths")], by.x = "Week", by.y="week", all.x=T)

deaths_2020 <- mortality_wide %>%
  select(LeeftijdOp31December,Week,`2020`) %>%
  spread(LeeftijdOp31December, `2020`)

deaths_weekly <- merge(deaths_2020, excess_deaths_wide, by = "Week")

week.now <- week(Sys.Date())-1 ## Which week?

excess_cbsmodel <- read.csv(paste0("workflow/excess_mortality/data/run_week",week.now,".csv"))

excess_cbsmodel <- excess_cbsmodel[which(excess_cbsmodel$model=="Dynamisch"),c("week","model","Gemiddeld")]

colnames(excess_cbsmodel) <- c("Week","Model","Oversterfte_CBS_DLModel")

df_cbsmodel <- excess_cbsmodel %>%
  mutate(Oversterfte_CBS_DLModel = c(0,diff(Oversterfte_CBS_DLModel))) # Calculate number of positive tests per day
df_cbsmodel[1,3] <- excess_cbsmodel[1,3]

df_cbsmodel$Oversterfte_CBS_DLModel <- round(df_cbsmodel$Oversterfte_CBS_DLModel,0)

colnames(deaths_weekly) <- c("Week","Totaal_Overleden","Overleden0_65","Overleden65_80","Overleden80+",
                                  "Oversterfte_Totaal","Oversterfte0_65","Oversterfte65_80","Oversterfte80+",
                                  "Oversterfte_Totaal_Gecorrigeerd","covid_sterfgevallen")

deaths_weekly <- merge(deaths_weekly, df_cbsmodel[,c("Week","Oversterfte_CBS_DLModel")],by="Week",all.x=T)

write.csv(deaths_weekly, file = "data-misc/excess_mortality/excess_mortality.csv", row.names = F)
