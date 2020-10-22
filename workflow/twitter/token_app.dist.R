## load rtweet
library(rtweet)

## authenticate via web browser
token <- create_token(
  app = "C19RedTeamStats",
  consumer_key = "API-key",
  consumer_secret = "API-key-secret")

# Get the user-access-token:
token[["credentials"]][["oauth_token"]]

# Get the user-access-token-secret
token[["credentials"]][["oauth_token_secret"]]

# Now duplicate the token_user.dist.R file, rename to token_username.R and add your access tokens there
