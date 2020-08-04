## Scraping LCPS data

require(jsonlite)
require(lubridate)
lcps.link <- "https://s3.eu-de.cloud-object-storage.appdomain.cloud/cloud-object-storage-lcps/news.json"

lcps <- read_json(path = lcps.link) %>% ## Read JSON link with LCPS news
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

lcps <- as.data.frame(t(lcps)) ## Transpose and turn into dataframe

lcps_ic <- data.frame() # Create empty dataframe for storage

lcps_ic <- as.data.frame(unlist(lcps$V1)) # Update: #patients in IC
lcps_ic$date <- unlist(lcps$V2) # Update: date of news

colnames(lcps_ic) <- c("patients_ic","date") #colnames

lcps_ic$parsed_date <- str_remove(lcps_ic$date, gsub("([A-Za-z]+).*", "\\1", lcps_ic$date)) # Strip day out of date (e.g., 'Maandag')

lcps_ic$date <- as.Date(lubridate::parse_date_time(lcps_ic$parsed_date, orders = c("ymd", "dmy", "mdy", "dm"))) # Parse date from string - check for errors
lcps_ic$patients <- round(parse_number(lcps_ic$patients_ic),0) # Parse number of patients out of string

lcps_ic <- lcps_ic[order(lcps_ic$date),] # Order by date

write.csv(lcps_ic, file = ("data-nice/data-lcps/lcps_today.csv")) # Write datafile

          