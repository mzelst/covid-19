temp = tail(list.files(path = "data-rivm/tests/",pattern="*.csv.gz", full.names = T),1)
tests <- fread(temp)

tests$Date_of_statistics <- as.Date(tests$Date_of_statistics)

tests.wide <- aggregate(Tested_with_result ~ Date_of_statistics, data = tests, FUN = sum)
tests.positive.wide <- aggregate(Tested_positive ~ Date_of_statistics, data = tests, FUN = sum)

tests.df <- merge(tests.wide,tests.positive.wide, by = c("Date_of_statistics"))
tests.df <- tests.df %>%
  mutate(pos.rate = Tested_positive/Tested_with_result*100)

tests.df$pos.rate.3d.avg <- round(frollmean(tests.df$pos.rate,3),1)
colnames(tests.df) <- c("date","values.tested_total","values.infected","values.infected_percentage","pos.rate.3d.avg")

write.csv(tests.df, file = "data-dashboards/percentage-positive-daily-national.csv",row.names = F)

tests$pos.rate <- tests$Tested_positive/tests$Tested_with_result*100
colnames(tests) <- c("Version","Date_of_report","date","Security_region_code","Security_region_name","values.tested_total",
                     "values.infected","values.infected_percentage")
write.csv(tests, file = "data-dashboards/percentage-positive-daily-safetyregion.csv",row.names = F)
