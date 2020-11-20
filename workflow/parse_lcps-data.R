require(tidyverse)

rm(list=ls())

lcps.data.original <- utils::read.csv('https://lcps.nu/wp-content/uploads/covid-19.csv', sep =',')
#lcps.data.original[163,] <- c("10-11-2020", 603, 476, 1842, 33, 180)
lcps.data.original[36,] <- c("16-10-2020",345,524,1208,35,219)
lcps.data.original[37,] <- c("15-10-2020",313,503,1213,30,201)
lcps.data.original[38,] <- c("14-10-2020",301,527,1174,27,225)
lcps.data.original[39,6] <- 236

# Order numbers: IC_Bedden_COVID, IC_Bedden_Non_COVID, Kliniek_Bedden, IC_Nieuwe_Opnames_COVID, Kliniek_Nieuwe_Opnames_COVID
lcps.data <- lcps.data.original %>%
  mutate(
    date = as.Date(Datum, tryFormats = c('%d-%m-%Y')),
    .before = Datum
  ) %>%
  mutate(
    Datum = NULL
  )



lcps.dailydata <- lcps.data %>%
  head(1)
lcps.date <- lcps.dailydata[['date']]

filename <- paste0('data-lcps/total/covid-19_', lcps.date, '.csv')
filename.daily <- paste0('data-lcps/data-per-day/covid-19_', lcps.date, '.csv')
filename.common <- 'data/lcps_by_day.csv'

write.csv(lcps.data.original, file=filename, row.names = F)
write.csv(lcps.dailydata, file = filename.daily, row.names = F)
write.csv(lcps.data, file = filename.common, row.names = F)

rm(list=ls())
