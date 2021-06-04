source("workflow/twitter/token_mzelst.R")
#source("workflow/excess_mortality_cbsmodel_2021.R")
#source("workflow/excess_mortality.R")

thisweek <- isoweek(Sys.Date())-1
startday.week <- substr(Sys.Date()-11,9,10)
endday.week <- substr(Sys.Date()-5,9,10)
rivm.startday <- substr(Sys.Date()-15,9,10)
rivm.endday <- substr(Sys.Date()-9,9,10)

## Build main tweet
tweet.main <- paste0("CBS heeft het aantal overlijdensgevallen bijgewerkt t/m week ",thisweek," van 2021. In dit draadje duid ik de sterfte per week + het aantal mensen dat is overleden door corona.")

posted_tweet <- post_tweet (
  tweet.main,
  token = token.mzelst,
  media = "data-misc/excess_mortality/plots_weekly_update/sterfte_perweek.png"
) ## Post tweet
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.main.id <- posted_tweet$id_str
tweet.last_id <- tweet.main.id

## Build information tweet

tweet.information <- paste0("2/ 

Ik kijk naar:

1) Oversterfte adhv historische gemiddeldes (tweet 3/4)
2) Sterfte door corona met een dynamisch linear model (DLM) (tweet 5)
3) Sterfte door corona met model gebaseerd op plaats van overlijden (tweet 6)

De uitleg kun je hier vinden: https://github.com/mzelst/covid-19/blob/master/data-misc/excess_mortality/REMARKS.md")

posted_tweet <- post_tweet (
  tweet.information,
  token = token.mzelst,
  in_reply_to_status_id = tweet.main.id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Build excess mortality (historical) tweet
excess_mortality <- read.csv("data-misc/excess_mortality/excess_mortality.csv")

tweet.excess.historical <- paste0("3/ De oversterfte in week ",thisweek," (",startday.week,"-",endday.week," mei):

1) Historisch gemiddelde: ",last(excess_mortality$Oversterfte_Totaal),"
2) Historisch gemiddelde (corr. leeftijd): ",last(excess_mortality$Oversterfte_Totaal_Gecorrigeerd),"
3) Methode CBS: ",last(excess_mortality$excess_cbs_method),"
4) Methode RIVM (",rivm.startday," mei - ",rivm.endday," mei): ",last(excess_mortality$excess_mortality_rivm),"

(grafieken CBS / RIVM)
")

posted_tweet <- post_tweet (
  tweet.excess.historical,
  token = token.mzelst,
  media = c("data-misc/excess_mortality/plots_weekly_update/overledenen-per-week.png","data-misc/excess_mortality/plots_weekly_update/sterfte_perweek_rivm.jpeg"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Retrieve link CBS mortality weekly
urls <- read.csv("data-misc/excess_mortality/links_cbs_mortality.csv")
u.cbs <- last(urls$urls)

## Load CBS website
webpage <- read_html(u.cbs)

## Tweet WLZ mortality
table.wlz <- as.data.frame(html_table(webpage)[2])
colnames(table.wlz) <- c("Year","Week","deaths_wlz","expected_deaths_wlz","ci_expected_deaths_wlz","deaths_other","expected_deaths_other","ci_expected_deaths_other")
table.wlz$deaths_wlz <- as.numeric(table.wlz$deaths_wlz)
table.wlz$expected_deaths_wlz <- as.numeric(table.wlz$expected_deaths_wlz)
table.wlz$deaths_other <- as.numeric(table.wlz$deaths_other)
table.wlz$expected_deaths_other <- as.numeric(table.wlz$expected_deaths_other)
table.wlz$excess_wlz <- table.wlz$deaths_wlz-table.wlz$expected_deaths_wlz
table.wlz$excess_other <- table.wlz$deaths_other-table.wlz$expected_deaths_other

table.wlz <- table.wlz %>%
  filter(excess_wlz != "NA")

table.wlz$excess_wlz_perc <- round(table.wlz$excess_wlz/table.wlz$expected_deaths_wlz*100,0)
wlz.text <- ifelse(last(table.wlz$excess_wlz_perc)<0,"minder","meer")
table.wlz$excess_other_perc <- round(table.wlz$excess_other/table.wlz$expected_deaths_other*100,0)
other.text <- ifelse(last(table.wlz$excess_other_perc)<0,"minder","meer")

tweet.wlz <- paste0("4/ Oversterfte Wlz en overige bevolking (CBS)

De sterfte bij Wlz-gebruikers (mensen in zorginstellingen) is ",abs(last(table.wlz$excess_wlz_perc)),"% ",wlz.text," dan verwacht.

De sterfte in de overige bevolking is ",abs(last(table.wlz$excess_other_perc)),"% ",other.text," dan verwacht.
")

posted_tweet <- post_tweet (
  tweet.wlz,
  token = token.mzelst,
  media = c("data-misc/excess_mortality/plots_weekly_update/overledenen-per-week-wlz.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Tweet DLM analyses

tweet.dlm <- paste0("5/ Het officiële aantal sterfgevallen voor week ",thisweek," is op dit moment ",last(excess_mortality$covid_sterfgevallen),".

De DLM methode schat het aantal corona-overledenen voor week ",thisweek," op ",last(excess_mortality$DLModel_week_estimate)," (95% betrouwbaarheidsinterval: ",last(excess_mortality$DLModel_lowerbound95),"-",last(excess_mortality$DLModel_upperbound95),").

De sterfte door corona tot nu toe is ",last(excess_mortality$Oversterfte_DLModel_cumul_mid)," (95% betrouwbaarheidsinterval: ",last(excess_mortality$Oversterfte_DLModel_cumul_low),"-",last(excess_mortality$Oversterfte_DLModel_cumul_high),").
")

posted_tweet <- post_tweet (
  tweet.dlm,
  token = token.mzelst,
  media = c("plots/2021_fig4.2.1.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Tweet Methode Marino analyses

deaths.comparison.tracker <- read.csv("corrections/death_week_comparisons.csv")
deaths.comparison.tracker <- deaths.comparison.tracker %>%
  filter(Year == 2021 & Week > 8 & Week <= thisweek)

cbs.deaths <- sum(excess_mortality$Covid_deaths_CBS_death_statistics,na.rm=T)
est.deaths <- sum(deaths.comparison.tracker$deaths_estimate_3)

tweet.newmodel <- paste0("6/ Een andere methode om de sterfte door corona te schatten beschreef ik laatst in dit draadje: https://twitter.com/mzelst/status/1390682590105985026

Het aantal sterfgevallen in week ",thisweek," aan de hand van deze methode is ",last(deaths.comparison.tracker$deaths_estimate_3),".

De sterfte door corona tot nu toe is ",cbs.deaths+est.deaths, ".
")

posted_tweet <- post_tweet (
  tweet.newmodel,
  token = token.mzelst,
  media = c("plots/sterfte_per_week_30K_totalen.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str


## Conclusie tweet

conclusie.tweet <- paste0("Conclusie: Na 8 weken van verhoogde sterfte was de sterfte in week 21 iets lager dan verwacht. De sterftecijfers wijzen er nu ook op dat het de goede kant opgaat.")

posted_tweet <- post_tweet (
  conclusie.tweet,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Eindnoot tweet

eindnoot.tweet <- paste0("Eindnoot

In onze repo houden we wekelijks deze analyses allemaal bij, zie hier: https://github.com/mzelst/covid-19/tree/master/workflow/excess_mortality

Ik zal wekelijks deze resultaten ook op Twitter bijwerken in een draadje.")

posted_tweet <- post_tweet (
  eindnoot.tweet,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Eindnoot tweet 2

eindnoot.tweet2 <- paste0("Eindnoot 2

De verschillen tussen de oversterfte cijfers van het CBS en RIVM leg ik hieronder uit. We spraken hier ook over met @rubenivangaalen die dat in detail uitlegt: https://signaalwaarde.nl/aflevering-6/

https://twitter.com/mzelst/status/1365703708579872775")

posted_tweet <- post_tweet (
  eindnoot.tweet2,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str