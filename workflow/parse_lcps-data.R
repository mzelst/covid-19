lcps.data.original <- utils::read.csv('https://lcps.nu/wp-content/uploads/covid-19.csv', sep =',')

# Order numbers: IC_Bedden_COVID, IC_Bedden_Non_COVID, Kliniek_Bedden, IC_Nieuwe_Opnames_COVID, Kliniek_Nieuwe_Opnames_COVID
lcps.data <- lcps.data.original %>%
  mutate(
    date = as.Date(Datum, tryFormats = c('%d-%m-%Y')),
    .before = Datum
  ) %>%
  mutate(
    Datum = NULL
  )

lcps.data$IC_Nieuwe_Opnames_COVID <- parse_number(lcps.data$IC_Nieuwe_Opnames_COVID)

lcps.condition <- head(lcps.data$Kliniek_Nieuwe_Opnames_COVID,1) < head(lcps.data$IC_Nieuwe_Opnames_COVID,1)
# Verify clinical beds and IC beds are correctly reported (not swapped around)
if (lcps.condition) {stop("The value is TRUE, so the script must end here")    
} else {
  
  lcps.dailydata <- lcps.data %>%
    head(1)
  lcps.date <- lcps.dailydata[['date']]
  
  filename <- paste0('data-lcps/total/covid-19_', lcps.date, '.csv')
  filename.daily <- paste0('data-lcps/data-per-day/covid-19_', lcps.date, '.csv')
  filename.common <- 'data/lcps_by_day.csv'
  
  lcps.data <- lcps.data[order(lcps.data$date),]
  
  lcps.data <- lcps.data %>%
    mutate(Totaal_bezetting = Kliniek_Bedden + IC_Bedden_COVID) %>%
    mutate(IC_Opnames_7d = frollmean(IC_Nieuwe_Opnames_COVID,7, na.rm = T)) %>%
    mutate(Kliniek_Opnames_7d = frollmean(Kliniek_Nieuwe_Opnames_COVID,7)) %>%
    mutate(Totaal_opnames = IC_Nieuwe_Opnames_COVID + Kliniek_Nieuwe_Opnames_COVID) %>%
    mutate(Totaal_opnames_7d = IC_Opnames_7d + Kliniek_Opnames_7d) %>%
    mutate(Totaal_IC = IC_Bedden_COVID + IC_Bedden_Non_COVID) %>%
    mutate(IC_opnames_14d = lag(IC_Opnames_7d,7)) %>%
    mutate(Kliniek_opnames_14d = lag(Kliniek_Opnames_7d,7)) %>%
    mutate(OMT_Check_IC = round(IC_Opnames_7d/IC_opnames_14d*100,0)) %>%
    mutate(OMT_Check_Kliniek = round(Kliniek_Opnames_7d/Kliniek_opnames_14d*100,0))
  
  
  lcps.data <- lcps.data[order(lcps.data$date, decreasing = T),]
  
  write.csv(lcps.data.original, file=filename, row.names = F)
  write.csv(lcps.dailydata, file = filename.daily, row.names = F)
  write.csv(lcps.data, file = filename.common, row.names = F)
  
}

rm(filename, filename.common, filename.daily, lcps.condition, lcps.date, lcps.dailydata, lcps.data.original)