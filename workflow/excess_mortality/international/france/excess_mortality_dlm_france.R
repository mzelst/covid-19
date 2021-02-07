## title: Script for estimating excess deaths from Corona
## author: Trond Husby
## date: 26.06.2020

##
## house keeping
##

## packages
library(cbsodataR)
library(dlm)
library(data.table)
library(ggplot2)
library(forecast)
library(rmarkdown)
library(knitr)
library(gridExtra)
library(readxl)

## for reproducibility
set.seed(123)

week.now <- 52

## helper functions

## 95% confidence interval
ci_5p <- function(val, side) {
  if (side == 'lwr') {
    quantile(val, probs = seq(0, 1, 0.025))[2] 
  } else if (side == 'upr') {
    quantile(val, probs = seq(0, 1, 0.025))[40] 
  }
}

## find week number from ts object
find_week <- function(var) {
  round(as.numeric((var - trunc(var))*52 + 1))
}

##
## read data
##

## registered corona deaths
## data from RIVM: downloaded 08.06.2020
#if(file.exists('/data/covid_deaths_dt.rds')) {
#  rivm_dt <- fread(paste0("https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/rivm_NL_covid19_national_by_date/rivm_NL_covid19_national_by_date_",Sys.Date()-16,".csv")
#  )[,
#    Datum := as.IDate(Datum)
#  ][,
#    week := week(Datum + 1)
#  ]
#  saveRDS(rivm_dt, '/data/covid_deaths_dt.rds')
#} else {
#  rivm_dt <- readRDS('/data/covid_deaths_dt.rds')
#}

####

## read cbsodata on weekly deaths
cbs_dt <- data.table(cbs_get_data('70895ned',
                                  Geslacht = "1100",
                                  LeeftijdOp31December = "10000",
                                  Perioden = has_substring("W") | has_substring("X"))
)[,
  timestamp := Sys.time()
]

## oversterfte from CBS/AMC model, https://www.cbs.nl/nl-nl/nieuws/2020/22/sterfte-in-coronatijd
cbs_oversterfte <- data.table(read_excel('workflow/excess_mortality/data/Berekening oversterfte CBS.xlsx', range = 'F3:I49', col_names = F))


##
## data wrangling
##

## aggregate corona deaths per week
#nl_dt <- rivm_dt[Type == 'Overleden',
#                 .(year = 2020, covid_deaths = sum(Aantal)),
#                 by = week
#]

nl_dt <- read.csv("corrections/deaths_perweek.csv")[,c("Week","weekdeath_today","Year")]
nl_dt <- data.table(nl_dt)
nl_dt <- nl_dt[,c(1,3,2)]
colnames(nl_dt) <- c("week","year","covid_deaths")
nl_dt$covid_deaths <- as.numeric(nl_dt$covid_deaths)
nl_dt$year <- as.numeric(nl_dt$year)
nl_dt <- nl_dt[c(1:(nrow(nl_dt)-1)),] ## Only use data up to week 30

## ts objects assume 52 weeks per year. Adjust the CBS data for 52 week/year 

## find number of days of each week
week_labels <- data.frame(attributes(cbs_dt$Perioden)$labels)
week_labels$lbl <- row.names(week_labels)
week_labels$w_length <- unlist(lapply(1:nrow(week_labels),
                                      function(x) {
                                        tmp <- gsub("[\\(\\)]", "",
                                                    regmatches(week_labels$lbl[x],
                                                               gregexpr("\\(.*?\\)", week_labels$lbl[x])
                                                    )[[1]]
                                        )
                                        if (length(tmp) > 0) {
                                          return(tmp)
                                        } else {
                                          return ('7 dagen')
                                        }
                                      }))
week_labels <- as.data.table(week_labels)

## add week length
setkeyv(week_labels, names(week_labels)[1])
setkey(cbs_dt, Perioden)
cbs_dt[week_labels,
       w_length := substr(i.w_length, 1, 1)
][,
  Perioden := gsub('X', 'W', Perioden)
]

cbs_dt <- setorder(cbs_dt, Perioden)

## move deaths from week 53 or 0 to week 52 or 1 depending on week length
cbs_dt[, ':=' (year = as.numeric(substr(Perioden, 1, 4)),
               week = as.numeric(substr(Perioden, 7, 8))
)
][,
  ':=' (week_update1 = Overledenen_1 + shift(Overledenen_1),
        week_update2 = Overledenen_1 + shift(Overledenen_1, -1)
  )
][week == 1 & w_length < 7,
  Overledenen_1 := week_update1
][week == 52 & w_length < 7,
  Overledenen_1 := week_update2
][,
  ':=' (week_update1 = NULL, week_update2 = NULL)
]

## merge corona deaths with overall deaths
nl_dt <- merge(cbs_dt[week %in% seq(1, 52),
                      .(cbs_deaths = Overledenen_1, year, week)
],
nl_dt,
by = c('year', 'week'),
all = T
)[!is.na(cbs_deaths)
][is.na(covid_deaths),
  covid_deaths := 0
][!(year == 2020 & week > 52),
]
####

cbs_oversterfte <- data.table(read_excel('workflow/excess_mortality/international/france/Berekening oversterfte CBS.xlsx', range = 'F3:I46', col_names = F))

nl_dt <- fread("meetings/deaths_france.csv")
nl_dt$year <- as.numeric(nl_dt$year)
nl_dt$covid_deaths <- as.numeric(nl_dt$covid_deaths)

## create time series objects
cbs_deaths_ts <- ts(nl_dt$cbs_deaths, start = c(2000, 1), frequency = 52)
covid_deaths_ts <- ts(as.numeric(nl_dt$covid_deaths),
                      start = c(2000, 1), frequency = 52)
covid_deaths_d <- ifelse(covid_deaths_ts == 0, 0, 1) # dummy

## add griepepidemien 2014:2019 to dummy variable. sources:
## https://www.rivm.nl/monitoring-sterftecijfers-nederland
## https://www.rivm.nl/bibliotheek/rapporten/2015-0042.pdf
## https://www.rivm.nl/nieuws/langdurige-griepepidemie-2017-2018-voorbij

## begin and end week of each epidemic
begin_end_list <- list(c(2017, 51, 2018, 14),
                       c(2016, 48, 2017, 10),
                       c(2016, 1, 2016, 10),
                       c(2014, 50, 2015, 17),
                       c(2012, 52, 2013, 16)
)

begin_end_griep <- rbindlist(lapply(begin_end_list,
                                    function(x) {
                                      data.table(begin_y = x[1],
                                                 begin_w = x[2],
                                                 end_y = x[3],
                                                 end_w = x[4]
                                      )
                                    })
)

## initiate dummy variable with all excess deaths periods
combined_d <- ifelse(covid_deaths_d == 1, 1, 0)

## set dummy equal to 1 in influenza weeks
for (i in 1:nrow(begin_end_griep)) {
  tmp <- window(time(covid_deaths_ts),
                start = c(begin_end_griep$begin_y[i],
                          begin_end_griep$begin_w[i]
                ),
                end = c(begin_end_griep$end_y[i],
                        begin_end_griep$end_w[i]
                )
  )
  combined_d <- ts(ifelse(time(combined_d) %in% tmp, 1, combined_d),
                   start = c(2000, 1), frequency = 52)
}

##
## models
##

##  selection of number of AR elements and number of harmonics
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  fit <- auto.arima(cbs_deaths_ts, xreg=fourier(cbs_deaths_ts, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}

## dynamic regression model

## initialisation
start_yr <- c(2000, 1)
level0 <- window(cbs_deaths_ts, start = start_yr)[1]
slope0 <- coefficients(lm(cbs_deaths_ts ~ covid_deaths_ts))[2]
init_val <- c(-1.571097963,  -4.373803720, 2.106434495,  0.837304892, 9.300978708)

## build function
covid_mod <- function(parm) {
  dlm <- dlmModReg(window(covid_deaths_ts, start = start_yr),
                   dW = exp(parm[c(2:3)]),
                   m0 = c(level0, slope0)
  ) +
    dlmModTrig(s=52, q = 2, dV = 0, dW = 0) + 
    dlmModARMA(ar=ARtransPars(parm[4]),
               ma=NULL,
               sigma2=exp(parm[5]))
  V(dlm) <- exp(parm[1])
  return(dlm)
}

## fit model
covid_fit <- dlmMLE(window(cbs_deaths_ts, start = start_yr),
                    parm = init_val,
                    build = covid_mod, hessian = T)

## create model with fitted parameters
covid_dlm <- covid_mod(covid_fit$par)

## kalman filter
covid_filt <- dlmFilter(window(cbs_deaths_ts, start = start_yr), covid_dlm)

## smoothing
covid_smooth <- dlmSmooth(covid_filt)

## draw samples from the posterior
covid_bs <- replicate(1000, dlmBSample(covid_filt))

## static regression model

## initialisation
init_val <- c(1.571097963,  4.373803720, 0.837304892, 9.300978708)

## build function
covid_mod_s <- function(parm) {
  dlm <-dlmModReg(window(covid_deaths_ts, start = start_yr),
                  dW = c(exp(parm[2]), 0), 
                  m0 = c(level0, slope0)) +
    dlmModARMA(ar=ARtransPars(parm[3]),
               ma=NULL,
               sigma2=exp(parm[4])) +
    dlmModTrig(s=52, q = 2, dW = 0)
  V(dlm) <- exp(parm[1])
  return(dlm)
}

## fit model
covid_fit_s <- dlmMLE(window(cbs_deaths_ts, start = start_yr),
                      parm = init_val,
                      build = covid_mod_s)

## create model with fitted parameters
covid_dlm_s <- covid_mod_s(covid_fit_s$par)

## kalman filter
covid_filt_s <- dlmFilter(window(cbs_deaths_ts, start = start_yr), covid_dlm_s)

## smoothing
covid_smooth_s <- dlmSmooth(covid_filt_s)

## draw samples from the posterior
covid_bs_s <- replicate(1000, dlmBSample(covid_filt_s))

## intervention model

## initialisation
start_yr <- start_yr <- c(2010, 1)
level0 <- window(cbs_deaths_ts, start = start_yr)[1]
slope0 <- coefficients(lm(window(cbs_deaths_ts, start = start_yr) ~
                            factor(window(combined_d, start = start_yr))
)
)[2]
init_val <- c(1.6057549,  3.8639435, 12.1585084,  0.8294332,  9.3343791)

## build function
covid_mod_d <- function(parm) {
  dlm <-dlmModReg(window(combined_d, start = start_yr),
                  dW = exp(parm[2 : 3]),
                  m0 = c(level0, slope0)) +
    dlmModARMA(ar=ARtransPars(parm[4]),
               ma=NULL,
               sigma2=exp(parm[5])) +
    dlmModTrig(s=52, q = 2, dW = 0)
  V(dlm) <- exp(parm[1])
  return(dlm)
}

## fit model
covid_fit_d <- dlmMLE(window(cbs_deaths_ts, start = start_yr),
                      parm = init_val,
                      build = covid_mod_d)

## create model with fitted parameters
covid_dlm_d <- covid_mod_d(covid_fit_d$par)

## kalman filter
covid_filt_d <- dlmFilter(window(cbs_deaths_ts, start = start_yr), covid_dlm_d)

## smoothing
covid_smooth_d <- dlmSmooth(covid_filt_d)

## draw samples from the posterior
covid_bs_d <- replicate(1000, dlmBSample(covid_filt_d))

##
##  Massage output data
##

## calculate estimated trend from the draws from the posterior
smooth_dt_dyn <- apply(covid_bs[,-2,], 3,
                       function(x) {
                         apply(dropFirst(x), 1,
                               function(y) {
                                 sum(y*covid_dlm$FF[,-2])
                               })
                       })

## find weekly betas
beta <- rbind(data.table(t = as.numeric(time(covid_filt$y)),
                         beta = dropFirst(covid_bs[,2,]),
                         covid_deaths = as.numeric(covid_filt$mod$X),
                         model = 'Dynamisch'
),
data.table(t = as.numeric(time(covid_filt_s$y)),
           beta = dropFirst(covid_bs_s[,2,]),
           covid_deaths = as.numeric(covid_filt_s$mod$X),
           model = 'Statisch'
),
data.table(t = as.numeric(time(covid_filt_d$y)),
           beta = dropFirst(covid_bs_d[,2,]),
           covid_deaths = as.numeric(covid_filt_d$mod$X),
           model = 'Interventie'
)
)

## transform to long, calculate excess deaths and add cbs/amc model
beta_long <- rbindlist(list(melt(beta,
                                 id.vars = c('t', 'model', 'covid_deaths')
)[,
  oversterfte := covid_deaths*value
],
data.table(t = 2020 + (cbs_oversterfte[,1]-1)/52,
           model = 'CBS/AMC',
           covid_deaths = NA,
           variable = 'beta',
           value = rep(1, nrow(cbs_oversterfte)),
           oversterfte = cbs_oversterfte[,4]
)
)
)[,
  week := find_week(t)
][,
  model := factor(model,
                  levels = c('Dynamisch',
                             'Statisch',
                             'Interventie',
                             'CBS/AMC')
  )
]

## calculate confidence intervals
beta_long[,
          ':=' (mid = mean(oversterfte),
                lwr = ci_5p(oversterfte, 'lwr'),
                upr = ci_5p(oversterfte, 'upr')
          ),
          by = c('t', 'model')
][model == 'CBS/AMC',
  c('lwr', 'upr') := NA
]

## calculate mean and CI interval 
totals <- beta_long[t >= 2020 & week %in% seq(9, 52),
                    .(week, cumsum(oversterfte)),
                    by=c('variable', 'model')
][,
  .(Laag = ifelse(model == 'CBS/AMC',
                  0,
                  ci_5p(V2, 'lwr')                        
  ),
  Gemiddeld = mean(V2),
  Hoog = ifelse(model == 'CBS/AMC',
                0,
                ci_5p(V2, 'upr')
  )
  ),
  by = c('model', 'week')
]

write.csv(totals, file = paste0("workflow/excess_mortality/international/france/dlm_2020.csv"))

##
## Figures
##

## Figure 2.1

## data
fig2.1_dt <- data.table(year = round(as.numeric(trunc(time(covid_filt$y)))),
                        week = find_week(as.numeric(time(covid_filt$y))),
                        smooth = apply(smooth_dt_dyn, 1, mean),
                        covid_sterfte = as.numeric(covid_filt$mod$X),
                        cbs_deaths = covid_filt$y,
                        lwr = apply(smooth_dt_dyn, 1, function(x) ci_5p(x, side = 'lwr')),
                        upr = apply(smooth_dt_dyn, 1, function(x) ci_5p(x, side = 'upr'))
)[!(year == 2020 & week >= 9),
  ':=' (
    smooth = NA, lwr = NA, upr = NA)
][year >= 2020,
]

write.csv(fig2.1_dt,file = "workflow/excess_mortality/international/france/fig2.1_dt.csv")

## create plot
ggplot(fig2.1_dt, aes(factor(week), cbs_deaths, group = 1)) +
  geom_ribbon(aes(ymin = 0, ymax = as.numeric(cbs_deaths) - covid_sterfte),
              fill = 'grey50', alpha = 0.4
  ) +
  geom_ribbon(aes(ymax = as.numeric(cbs_deaths),
                  ymin = as.numeric(cbs_deaths) - covid_sterfte),
              fill = 'grey10', alpha = 0.4
  ) +
  geom_ribbon(aes(ymax = upr,
                  ymin = lwr),
              fill = 'red', alpha = 0.4
  ) +    
  geom_line(aes(y=smooth), lty = 'dotted') +
  coord_cartesian(ylim = c(1500, 5000)) +
  xlab('Week') +
  ylab('') + 
  theme_bw()
ggsave('workflow/excess_mortality/international/france/fig2.1.png')


## Figure 2.2

## data for figure
fig2.2_dt <- totals[model == 'Dynamisch']

write.csv(fig2.2_dt,file = "workflow/excess_mortality/international/france/fig2.2_dt.csv")

## create plot
ggplot(fig2.2_dt, aes(factor(week), Gemiddeld)) +
  geom_col(alpha = 0.4) +
  geom_errorbar(aes(ymax = Hoog, ymin = Laag), col = 'red', alpha = 0.4) +
  geom_text(aes(label = round(Gemiddeld)), vjust = 3.0) +
  xlab('Week') +
  ylab('Oversterfte cumulatief') +
  theme_bw()
ggsave('workflow/excess_mortality/international/france/fig2.2.png')

## Figure 4.1.1
fig4.1.1_dt <- beta_long[model == 'Dynamisch' & t >= 2020 & week >= 8,
                         .(mid = mean(value),
                           upr = ci_5p(value, side = 'upr'),
                           lwr = ci_5p(value, side = 'lwr')
                         ),
                         by = week
]

write.csv(fig4.1.1_dt,file = "workflow/excess_mortality/international/france/fig4.1.1_dt.csv")

## create plot
ggplot(fig4.1.1_dt, aes(factor(week), mid, group = 1)) +
  geom_line() +
  geom_errorbar(aes(ymax = upr, ymin = lwr), col = 'red', alpha = 0.4) + 
  xlab('Week') +
  ylab('Beta') + 
  theme_bw()
ggsave('workflow/excess_mortality/international/france/fig4.1.1.png')

## Figure 4.1.2

## data
fig4.1.2_dt <- data.table(year = round(as.numeric(trunc(time(covid_filt$y)))),
                          week = find_week(time(covid_filt$y)),
                          cbs_deaths = covid_filt$y,
                          mid = apply(smooth_dt_dyn, 1, mean),
                          lwr = apply(smooth_dt_dyn, 1, function(x) ci_5p(x, side = 'lwr')),
                          upr = apply(smooth_dt_dyn, 1, function(x) ci_5p(x, side = 'upr'))
)[!(year == 2020 & week >= 9),
  ':=' (smooth = NA, lwr = NA, upr = NA)
][year > 2009 & week <= 52,
]

write.csv(fig4.1.2_dt,file = "workflow/excess_mortality/international/france/fig4.1.2_dt.csv")

## plot
ggplot(fig4.1.2_dt,
       aes(as.factor(week), mid, group = year)) +
  geom_line(aes(y = cbs_deaths, col = ifelse(year >= 2020, 'grey', 'black'))) +
  geom_line(aes(lty = ifelse(year >= 2020, 'solid', 'dotted'), col = ifelse(year >= 2020, 'grey', 'black'))) +
  geom_ribbon(aes(ymax = upr, ymin = lwr), fill = 'red', alpha = 0.1) +
  scale_color_manual(values=c("grey", "black")) + 
  guides(lty = F, col = F) + 
  xlab('week') +
  ylab('') + 
  theme_bw()
ggsave('workflow/excess_mortality/international/france/fig4.1.2.png')

## figure 4.2.1
fig4.2.1_dt <-beta_long[t >= 2020 & week >= 8,
                        lapply(.SD, function(x) unique(x)),
                        .SDcols = c('mid', 'lwr', 'upr'),
                        by = c('week', 'model')
]

write.csv(fig4.2.1_dt,file = "workflow/excess_mortality/international/france/fig4.2.1_dt.csv")

fig4.2.1_dt <- fig4.2.1_dt %>%
  dplyr::filter(model == "Dynamisch")

## plot
ggplot(fig4.2.1_dt, aes(factor(week), mid, group = 1)) +
  geom_col(size = 4, position = 'dodge') +
  geom_errorbar(aes(ymin = lwr, ymax = upr, col = ifelse(model == 'CBS', NA, 'red')), alpha = 0.4) +
  ## scale_x_continuous(breaks = as.numeric(time(window(cbs_deaths_ts, start = c(2020, 1)))),
  ##                    labels = seq(1, nl_dt[year == 2020, max(week)])
  ##                    ) +
  scale_colour_manual(values = c('red', NA)) +
  guides(colour = F) + 
  ylab('Oversterfte') +
  xlab('Week') + 
  theme_bw()
ggsave('workflow/excess_mortality/international/france/fig4.2.1.png')

## figure 4.2.2
fig4.2.2_dt <- melt(totals[week == 52][,-'week'], id.vars = 'model')
fig4.2.2_dt <- fig4.2.2_dt %>%
  dplyr::filter(model == "Dynamisch")

write.csv(fig4.2.2_dt,file = "workflow/excess_mortality/international/france/fig4.2.2_dt.csv")

## plot
ggplot(fig4.2.2_dt, aes(variable, value)) +
  geom_col(alpha = 0.4) + 
  geom_text(aes(label = round(value)), vjust = 3) + 
  xlab('') +
  ylab('Totale oversterfte') + 
  theme_bw()
ggsave('workflow/excess_mortality/international/france/fig4.2.2.png')
