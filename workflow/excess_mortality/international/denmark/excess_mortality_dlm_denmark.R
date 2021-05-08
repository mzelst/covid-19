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

## oversterfte from CBS/AMC model, https://www.cbs.nl/nl-nl/nieuws/2020/22/sterfte-in-coronatijd
cbs_oversterfte <- data.table(read_excel('workflow/excess_mortality/data/Berekening oversterfte CBS.xlsx', range = 'F3:I49', col_names = F))


nl_dt <- read.csv("workflow/excess_mortality/international/denmark/deaths_denmark.csv")
nl_dt$year <- as.numeric(nl_dt$year)
nl_dt$covid_deaths <- as.numeric(nl_dt$covid_deaths)
nl_dt <- nl_dt %>%
  filter(week < 53) %>%
  filter(year < 2021)


## create time series objects
cbs_deaths_ts <- ts(nl_dt$cbs_deaths, start = c(2007, 1), frequency = 52)
covid_deaths_ts <- ts(as.numeric(nl_dt$covid_deaths),
                      start = c(2007, 1), frequency = 52)
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
                   start = c(2007, 1), frequency = 52)
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
start_yr <- c(2007, 1)
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
totals <- beta_long[t >= 2020 & week %in% seq(11, 52),
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

write.csv(totals, file = paste0("workflow/excess_mortality/international/denmark/dlm_2020.csv"))
