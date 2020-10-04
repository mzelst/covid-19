require(tidyverse)
require(tabulizer)

report <- "https://www.rivm.nl/sites/default/files/2020-08/COVID-19_WebSite_rapport_wekelijks_20200721_1135.pdf"
report <- "https://www.rivm.nl/sites/default/files/2020-08/COVID-19_WebSite_rapport_wekelijks_20200728_1052.pdf"
report <- "https://www.rivm.nl/sites/default/files/2020-08/COVID-19_WebSite_rapport_wekelijks_20200804_1306.pdf"
report <- "https://www.rivm.nl/sites/default/files/2020-08/COVID-19_WebSite_rapport_wekelijks_20200811_1158_0.pdf"
report <- "https://www.rivm.nl/sites/default/files/2020-08/COVID-19_WebSite_rapport_wekelijks_20200818_1224.pdf"
report <- "https://www.rivm.nl/sites/default/files/2020-08/COVID-19_WebSite_rapport_wekelijks_20200825_1217.pdf"
report <- "https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200901_1353_1.pdf"
report <- "https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200908_1159.pdf"
report <- "https://www.rivm.nl/sites/default/files/2020-09/COVID-19_WebSite_rapport_wekelijks_20200929_1159_0.pdf"

area.table <- locate_areas(report,
                          pages=c(29))

age_tests <- extract_tables(report,
                           output = "data.frame",
                           pages = c(29),
                           area = area.table,
                           guess=FALSE)
age_tests <- do.call(rbind,age_tests)
age_tests <- age_tests[c(2:(nrow(age_tests))),]

age_tests[,c(2:7)] <- sapply(age_tests[,c(2:7)],as.numeric)
age_tests$Positief <- age_tests$Aantal + age_tests$Aantal.2
age_tests$Tests <- age_tests$Aantal.1 + age_tests$Aantal.3
age_tests <- age_tests[,c(1,8,9)]
age_tests$Percentage_Positief <- age_tests$Positief/age_tests$Tests
age_tests$Week <- isoweek("2020-10-03")-1

colnames(age_tests) <- c("Leeftijd","Positief_getest","Aantal_tests","Percentage_positief","Week")
write.csv(age_tests,file="age_tests.csv")

age_tests_df <- rbind(age_tests_df,age_tests)

#age_test_df <- merge(age_tests,age_test_df, by = "Leeftijd")
#colnames(age_test_df) <- c("Leeftijd","Positief_36","Aantal_tests_36","Positief_35","Aantal_tests_35","Positief_34","Aantal_tests_34")
#age_test_df$Perc_34 <- round(age_test_df$Positief_34/age_test_df$Aantal_tests_34*100,2)
#age_test_df$Perc_35 <- round(age_test_df$Positief_35/age_test_df$Aantal_tests_35*100,2)
#age_test_df$Perc_36 <- round(age_test_df$Positief_36/age_test_df$Aantal_tests_36*100,2)


#col_order <- c("Leeftijd","Positief_34","Aantal_tests_34","Perc_34","Positief_35","Aantal_tests_35","Perc_35","Positief_36","Aantal_tests_36","Perc_36")
#age_test_df <- age_test_df[, col_order]

age_tests_df <- age_tests_df %>%
  group_by(Leeftijd) %>%
  arrange(Week, .by_group = TRUE) %>%
  mutate(Toename_tests = Aantal_tests - lag(Aantal_tests))

age_tests_df <- age_tests_df %>%
  group_by(Leeftijd) %>%
  arrange(Week, .by_group = TRUE) %>%
  mutate(Toename_positief = Positief_getest - lag(Positief_getest))

age_tests_df$Percentage_positief_week <- round(age_tests_df$Toename_positief/age_tests_df$Toename_tests*100,3)

write.csv(age_tests_df,file = "data-dashboards/age_tests_week.csv", row.names = FALSE)

age_tests_df$Percentage_positief_week <- ifelse(age_tests_df$Percentage_positief_week<0, 0, age_tests_df$Percentage_positief_week)

age_tests_df %>%
  ggplot( aes(x=Week, y=Percentage_positief_week, group=Leeftijd, color=Leeftijd)) +
  geom_line() + 
  ggsave("plots/age_tests_week.png", width = 12, height=10)
