require(tidyverse)
require(data.table)

# Municipality data

temp = list.files(path = "data-rivm/municipal-datasets/",pattern="*.csv", full.names = T)
myfiles = lapply(temp, read.csv)

temp = list.files(path = "data-rivm/municipal-datasets/",pattern="*.csv")

myfiles <- mapply(cbind, myfiles, "name_dataset"=temp,SIMPLIFY = F)

datefunction <- function(x) {
  x[x$Date_of_report == last(x$Date_of_report),]
} ## Function for cases per day

res <- lapply(myfiles, datefunction)

df <- map_dfr(res, ~{
  .x
})

df$date <- as.Date(df$Date_of_report)

df <- df %>%
  filter(Municipality_name != "") %>%
  select(Municipality_name, date, Total_reported)

data_wide <- reshape(df, direction="wide",
                     timevar="date",
                     idvar="Municipality_name")

# Calc diffs

col.start.diff <- ncol(data_wide)+1

dates.lead <- names(data_wide)[3:ncol(data_wide)] ## Set lead colnames for diff
dates.trail <- names(data_wide)[2:(ncol(data_wide)-1)] ## Set trail colnames for diff

# Calculate moving difference between cases per day
data_wide[paste0("diff",seq_along(dates.lead)+1,seq_along(dates.trail))] <- data_wide[dates.lead] - data_wide[dates.trail]

week <- last(colnames(data_wide), n = 7)

data_wide$weeksum <- rowSums(data_wide[,week])

