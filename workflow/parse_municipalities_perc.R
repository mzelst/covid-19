library(tidyverse)

tweets.enabled <- TRUE
filter.municipality <- ""

source("workflow/twitter/token_edwinveldhuizen.R")
source("workflow/twitter/token_spamedwin.R")

Sys.setlocale("LC_TIME", "nl_NL")
counter <- 0

format_custom_number <- function(data, plus = FALSE, format = "%s") {
  return( sapply(data, function(value){
    formatted_value <- formatC(value, format="f", big.mark=".", decimal.mark=",", digits=0)
    plus_or_minus <- ifelse(value > 0, '+', '-')
    formatted_value <- ifelse(plus, paste( plus_or_minus, formatted_value, sep = ''), formatted_value)
    formatted_value <- sprintf(format, formatted_value)
    formatted_value <- ifelse(value == 0, '-', formatted_value)
    return(formatted_value)
  }))
}

tweet_detailed <- function(data){
  int.hosp <- dat.hosp[dat.hosp$Municipality_code==data$Municipality_code, "d0"]
  int.deaths <- dat.deaths[dat.deaths$Municipality_code==data$Municipality_code, "d0"]
  
  if(int.deaths <= 5){
    return();
  }
  
  counter <<- counter + 1
  cat(paste('tweet', counter, '/ 300 :', data$municipality, "\n"))
    
  tweet <- sprintf("%s %s

%s inwoners

Hiervan is (nog maar) %.1f%% positief getest, maar al wél meer dan %s patiënten opgenomen (geweest) en %s overleden.

%s opa's, oma's, vaders, moeders, vrienden, collega's en zelfs zoons of dochters...

#CodeZwart

[#COVID19NL %s]",
  intToUtf8(0x2B1B), 
  data$municipality,
  format_custom_number(data$population),
  data$d0 / data$population * 100,
  format_custom_number(int.hosp),
  format_custom_number(int.deaths),
  format_custom_number(int.deaths),
  tweet.date
  )
  Encoding(tweet) <- "UTF-8"
  cat(paste(tweet, "\n"))
  
  if (tweets.enabled == TRUE) { 
    posted_tweet <<- post_tweet(tweet, 
      in_reply_to_status_id = reply_id, ## Post reply
      token = token.spamedwin,
      auto_populate_reply_metadata = TRUE
    )
    # posted_tweet <<- fromJSON(rawToChar(posted_tweet$content))
    # reply_id <<- posted_tweet$id_str
  }
}

dat.deaths <- read.csv("data/municipality-deaths-today-detailed.csv", fileEncoding = "UTF-8")
dat.hosp <- read.csv("data/municipality-hospitalisations-today-detailed.csv", fileEncoding = "UTF-8")
dat.cases <- read.csv("data/municipality-today-detailed.csv", fileEncoding = "UTF-8") %>%
  filter(Municipality_code != "")

if (filter.municipality != "") {
  dat.cases <- filter(dat.cases, municipality == filter.municipality)
}else{
  dat.cases <- dat.cases %>%
    arrange(
      match(Municipality_code, c("GM0088", "GM0096", "GM0060", "GM0093", "GM0448", "GM0180", "GM0744", "GM0244", "GM0946")),
      desc(population)
    ) %>%
    arrange(municipality) %>%
    tail(141)
}



used_date <- as.Date(last(dat.cases$date))
tweet.date <- used_date %>%
  format('%d %b') %>%
  str_to_title()  %>%
  str_replace( '^0', '')

tweet.date7d <- (used_date - 7) %>%
  format('%d %b') %>%
  str_replace( '^0', '')

tweet <- sprintf("Gedetailleerd overzicht van alle gemeentes %s

Zoeken kan in uw Twitter zoekbalk:
'from:spamedwin Rotterdam'
%s

#CodeZwart Edition

[%s]", 
                 intToUtf8(0x1F447), 
                 "https://twitter.com/search?q=from%3A%40spamedwin%20Rotterdam%20%23CodeZwart&src=typed_query&f=live",
                 tweet.date
)
Encoding(tweet) <- "UTF-8"

if (FALSE && tweets.enabled == TRUE) { 
  posted_tweet <- post_tweet(tweet, token = token.edwinveldhuizen)
  posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
  reply_id <- posted_tweet$id_str
}

by(dat.cases, 1:nrow(dat.cases), tweet_detailed)
