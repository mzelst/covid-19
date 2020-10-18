## load rtweet
library(rtweet)

## authenticate via known access_token
token.username <- create_token(
  app = "C19RedTeamStats",
  consumer_key = "API-key",
  consumer_secret = "API-key-secret",
  access_token = "user-access-token",
  access_secret = "user-access-token-secret")
