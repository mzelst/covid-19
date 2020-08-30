require(base)
require(utils)
require(stats)
require(rmarkdown)
require(cowplot)
require(tidyverse)
require(rjson)
require(rtweet)
require(data.table)
require(git2r)
twit.auth <- read.csv("twitter_auth.csv")
token <- create_token(app = twit.auth[1,2],
                      consumer_key = twit.auth[2,2],
                      access_token = twit.auth[3,2],
                      consumer_secret = twit.auth[4,2],
                      access_secret = twit.auth[5,2])

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])
repo <- init()