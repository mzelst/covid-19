library(tidyverse)

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
  counter <<- counter + 1
  cat(paste('tweet', counter, '/ 300 :', data$municipality, "\n"))
  
  int.hosp <- dat.hosp[dat.hosp$Municipality_code==data$Municipality_code, "increase_14d"]
  int.deaths <- dat.deaths[dat.deaths$Municipality_code==data$Municipality_code, "increase_14d"]
  
  text.hosp <- sprintf(ifelse(int.hosp == 1, "%s opname\n", "%s opnames\n"), format_custom_number(int.hosp, TRUE) )
  text.deaths <- sprintf(ifelse(int.deaths == 1, "%s overlijden\n", "%s overlijdens\n"), format_custom_number(int.deaths, TRUE))
  
  text.twoweeks <- sprintf("\nDaarbij in de laatste 2 weken:\n%s%s",
     ifelse(int.hosp > 0, text.hosp, ''),
     ifelse(int.deaths > 0, text.deaths, '')
  )
  text.twoweeks <- ifelse(int.hosp > 0 || int.deaths > 0, text.twoweeks, '')
  
  more_or_less <- (data$d0 - data$d7) / (data$d7 - data$d14)
  tweet <- sprintf("%s %s %s

%s sinds gisteren
%s sinds 1 dec
%s sinds 7 dagen (%s)
Wat %s is dan de %s in de 7d ervoor

%s inwoners maakt dat
%s %.1f / 100.000 / 7d
%s
[#COVID19NL %s]",
    data$color, 
    data$municipality,
    data$growth,
    ifelse(data$increase_1d == 0, 0, format_custom_number(data$increase_1d, TRUE)),
    format_custom_number(data$current, TRUE),
    format_custom_number(data$increase_7d, TRUE),
    tweet.date7d, 
    ifelse(    more_or_less >= 2, "fors meer",
       ifelse( more_or_less >= 1, "meer",
       ifelse( more_or_less == 1, "evenveel",  
       ifelse( more_or_less < 0.4, "fors minder", 
                                   "minder" 
       )))),
    format_custom_number(data$d7 - data$d14, TRUE),
    format_custom_number(data$population),
    data$color, 
    data$rel_increase_7d,
    text.twoweeks,
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
    head(300) %>%
    arrange(municipality)
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
'from:spamedwin Urk'
%s

[%s]", 
  intToUtf8(0x1F447), 
  "https://twitter.com/search?q=from%3A%40spamedwin%20Urk&src=typed_query&f=live",
  tweet.date
)
Encoding(tweet) <- "UTF-8"

if (tweets.enabled == TRUE) { 
  posted_tweet <- post_tweet(tweet, token = token.edwinveldhuizen)
  posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
  reply_id <- posted_tweet$id_str
}

by(dat.cases, 1:nrow(dat.cases), tweet_detailed)
