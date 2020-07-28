require(tidyverse)
require(data.table)

## Test code - Only use latest dataset

temp = list.files(path = "data-rivm/municipal-datasets/",pattern="*.csv", full.names = T) ## Pull names of all available datafiles

dat <- read.csv(last(temp)) ## Take last filename from the folder, load csv
dat$date <- as.Date(dat$Date_of_report) ## character into Date class
filter.date <- Sys.Date()-28 ## Create filter for last four weeks +1

dat <- dat %>%
  filter(Municipality_name != "") %>% # Filter observations without municipal name
  filter(date >= filter.date) %>% # Filter last four weeks 
  select(Municipality_name, date, Total_reported) # Select municipality, cases reported

dat.wide <- reshape(dat, direction="wide", # Reshape file into wide format -- columns will be dates which report total cases on date
                    timevar="date",
                    idvar="Municipality_name")

dat.wide$increase <- dat.wide[,ncol(dat.wide)]-dat.wide[,(ncol(dat.wide)-1)] # Calculate increase since last day 
dat.wide$increase.week <- dat.wide[,(ncol(dat.wide)-1)]-dat.wide[,(ncol(dat.wide)-8)] # Calculate increase since last week

mun.pop <- read.csv("misc/municipalities-population.csv")

dat.amsterdam <- dat.wide %>%
  filter(Municipality_name == "Amsterdam")

dat.amsterdam.t <- t(dat.amsterdam[,c(2:138)])
ams.diff <- data.frame(diff(dat.amsterdam.t))
positivetests <- dat.amsterdam.t[2:137]
date <- unique(dat$date)[2:137]
ams <- cbind(positivetests,ams.diff, date)

colnames(ams) <- c("cases","positivetests","date")


cases <- ams %>%
  ggplot(aes(x=date, y=positivetests)) + 
  geom_line(aes(y = positivetests, color = "Toename besmettingen per dag (incl. correcties)"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Besmettingen per dag",
       color = "Legend") +
  ggtitle("Meldingen van geconstateerde besmettingen")

cases


require(emojifont)
emoji(search_emoji('smile'))

emoji('smiley')


set.seed(2016-03-09)
fa <- fontawesome(c('fa-github', 'fa-weibo', 'fa-twitter', 'fa-android', 'fa-coffee'))
d <- data.frame(x=rnorm(20),
                y=rnorm(20),
                label=sample(fa, 20, replace=T))

ggplot(d, aes(x, y, color=label, label=label)) +
  geom_text(family='fontawesome-webfont', size=20)+
  xlab(NULL)+ylab(NULL) +
  theme(legend.text=element_text(family='fontawesome-webfont')) + 
  ggsave("plots/emojis.png",width=15, height = 4)




test <- merge(mun.pop, dat.wide, by = "Municipality_name")



all.data <- read.csv("data/all_data.csv")


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


