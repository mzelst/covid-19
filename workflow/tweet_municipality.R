library(tidyverse)

Sys.setlocale("LC_TIME", "nl_NL")

tweet.date <- Sys.Date() %>%
  format('%d %b') %>%
  str_to_title()  %>%
  str_replace( '^0', '')

get_reply_id <- function(rel_increase) {
  my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
  reply_id <- my_timeline$status_id[1] ## Status ID for reply
  return(reply_id)
}

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
  more_or_less <- (data$d0 - data$d7) / (data$d7 - data$d14)
  
  tweet <- sprintf("%s %s %s %s ( w: %s ) %s
  
%s sinds gisteren
%s sinds 1 september
%s sinds 7 dagen ( %s )
Wat %s is dan de %s in de 7 dagen ervoor
%s inwoners maakt dat %s %.1f / 100.000 / 7d
                   
[%s]",
    format_custom_number(data$increase_1d, TRUE),
    data$color, 
    data$municipality,
    format_custom_number(data$current, FALSE),
    format_custom_number(data$increase_7d, TRUE),
    data$growth,
    ifelse(data$increase_1d == 0, 0, format_custom_number(data$increase_1d, TRUE)),
    format_custom_number(data$current, TRUE),
    format_custom_number(data$increase_7d, TRUE),
    format(used_date - 7, '%d-%m-%Y'), 
    ifelse(    more_or_less >= 2, "fors meer", 
       ifelse( more_or_less >= 1, "meer",  
       ifelse( more_or_less < 0.4, "fors minder", 
                                   "minder" 
       ))),
    format_custom_number(data$d7 - data$d14, TRUE),
    format_custom_number(data$population),
    data$color, 
    data$rel_increase_7d, 
    tweet.date
  )
  Encoding(tweet) <- "UTF-8"
  post_tweet(tweet, 
    in_reply_to_status_id = get_reply_id() ## Post reply
  ) 
}

dat.cases <- read.csv("data/municipality-today-detailed.csv", fileEncoding = "UTF-8") %>%
  arrange(municipality)

used_date <- as.Date(last(dat.cases$date))

tweet <- "Gedetailleerd overzicht van alle gemeentes %s"
tweet <- sprintf(tweet, intToUtf8(0x1F447))
Encoding(tweet) <- "UTF-8"
post_tweet(tweet)

by(dat.cases, 1:nrow(dat.cases), tweet_detailed)




