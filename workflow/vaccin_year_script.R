require(tidyverse)
require(discordr)
require(tidyRSS)

source("covid-19/workflow/twitter/token_mzelst.R")

repeat {
  Sys.sleep(120)
  
  test <- tidyfeed("https://www.rivm.nl/nieuws/rss.xml")
  test$vaccin <- ifelse(str_extract_all(test$item_title, "coronavaccinatie")=="coronavaccinatie","vaccin","niets")
  test <- test %>% filter(vaccin == "vaccin")
  test$years <- purrr::map_dbl(str_extract_all(str_replace_all(test$item_description,",", ""), "\\d+"), ~max(as.numeric(.x)))
  
  current.year.rivm <- first(test$years)
  
  u <- "https://www.rijksoverheid.nl/onderwerpen/coronavirus-vaccinatie/prikuitnodiging-en-afspraak"
  webpage <- read_html(u)
  
  text <- webpage %>%
    html_nodes("strong") %>%
    html_text()
  
  current.year.rijksoverheid <- parse_number(text[1])
  
  current.year <- ifelse(current.year.rijksoverheid >= current.year.rivm, current.year.rijksoverheid,current.year.rivm)
  
  last.year <- 1982
  if (current.year > last.year){
    years <- last.year:current.year
    vaccin.year.message <- paste0("PING PING PING - Mensen uit ",years[2]," zijn aan de beurt voor vaccinatie!")
    vaccin.year2.message <- paste0("PING PING PING - Mensen uit ",years[2]," en ",years[3]," zijn aan de beurt voor vaccinatie!")
    message <- ifelse(length(years) > 2, vaccin.year2.message,vaccin.year.message)
    send_webhook_message(message)
    break
  }
}



