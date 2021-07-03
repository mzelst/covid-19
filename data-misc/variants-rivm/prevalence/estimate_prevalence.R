# ANALYSIS OF GROWTH ADVANTAGE OF DIFFERENT SARS-CoV2 VARIANTS OF CONCERN IN NETHERLANDS ####
# Tom Wenseleers

# Data: baseline surveillance whole genome sequencing, https://data.rivm.nl/covid-19/COVID-19_varianten.csv
# downloaded on 25th of June

# Tom Wenseleers, last update 25 JUNE 2021

library(lme4)
library(splines)
library(purrr)
library(readxl)
library(effects)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(dplyr)
library(tidyr)     
library(readr)
library(scales)
library(quantreg)
library(gamm4)
# install from https://github.com/tomwenseleers/export
#library(devtools)
#devtools::install_github("tomwenseleers/export")
library(export) 
library(afex)
library(dfoptim)
library(optimx)
library(lubridate)
library(zoo)
library(gridExtra)
library(sf)
library(broom)
# unloadNamespace("emmeans") # install latest development version of emmeans to add support for mblogit models & to fix bug in v1.5.4 with multinom models
library(devtools)
#remotes::install_github("rvlenth/emmeans", dependencies = TRUE, force = TRUE)
library(emmeans)
library(broom)
library(nnet)
#devtools::install_github("melff/mclogit",subdir="pkg") # install latest development version of mclogit, to add emmeans support
library(mclogit)

setwd("data-misc/variants-rivm/prevalence")

#dat="prevalence" # desired path in //data
#suppressWarnings(dir.create(paste0(".//data-misc//variants-rivm//",dat)))
# filedate = as.Date(gsub("_","-",dat)) # file date
# filedate_num = as.numeric(filedate)
# today = as.Date(Sys.time()) # we use the file date version as our definition of "today"
today = as.Date(Sys.Date())
today_num = as.numeric(today)

set_sum_contrasts() # we use effect coding for all models

# 1. ASSESSMENT OF GROWTH RATE ADVANTAGES OF VOCs B.1.1.7 (alpha), B.1.351 (beta), P.1 (gamma), B.1.617.1 (kappa) & B.1.617.2 (delta)
# IN NETHERLANDS BASED ON BASELINE SURVEILLANCE SEQUENCING & VOC PCR DATA ####
# (baseline surveillance, i.e. randomly sampled, excluding travellers & surge testing)
#nl_baseline = read.csv("https://data.rivm.nl/covid-19/COVID-19_varianten.csv", sep=";")

## Download data ##

require(tidyverse)
require(data.table)

nl_baseline <- read.csv("https://data.rivm.nl/covid-19/COVID-19_varianten.csv",sep = ";")
nl_baseline$Date_of_statistics_week_start <- as.Date(nl_baseline$Date_of_statistics_week_start)

#nl_baseline <- read.csv("https://raw.githubusercontent.com/mzelst/covid-19/master/data-misc/variants-rivm/prevalence_variants.csv")
#nl_baseline <- nl_baseline[,-c(5,10:19)]
#colnames(nl_baseline) <- c("Week","Jaar","Sample_size","B.1.1.7 (alpha)", "B.1.351 (beta)", "P.1 (gamma)", "B.1.617.2 (delta)", "B.1.617.1 (kappa)")

#nl_baseline <- nl_baseline %>%
##  gather(Variant_name, Variant_cases, `B.1.1.7 (alpha)`:`B.1.617.1 (kappa)`) %>%
#  filter(Jaar > 2020)

#nl_baseline$Date_of_statistics_week_start <- as.Date(paste(2021, nl_baseline$Week, 1, sep="-"), "%Y-%U-%u")
#nl_baseline$Variant_code <- gsub( " .*$", "", nl_baseline$Variant_name)
#nl_baseline$Variant_name <- recode(nl_baseline$Variant_name,"Alfa* (B.1.1.7) (Verenigd Koninkrijk)" = "Alpha",
#                                   "Beta (B.1.351) (Zuid-Afrika)" = "Beta",
#                                   "Gamma (P.1) (Brazilië)" = "Gamma",
#                                   "Delta (B.1.617.2) (India)" = "Delta",
#                                   "Kappa (B.1.617.1) (India)"  = "Kappa")

#nl_baseline$Date_of_statistics_week_start <- dmy(nl_baseline$Date_of_statistics_week_start)

nl_baseline$collection_date = as.Date(nl_baseline$Date_of_statistics_week_start)+3.5 # we use week midpoint
nl_baseline$variant = paste0(nl_baseline$Variant_code, " (", tolower(nl_baseline$Variant_name),")")

selected_variants = c("B.1.1.7 (alpha)", "B.1.351 (beta)", "P.1 (gamma)", "B.1.617.1 (kappa)", "B.1.617.2 (delta)")

nl_baseline = nl_baseline[nl_baseline$variant %in% selected_variants,]
nl_baseline = nl_baseline[,c("collection_date","variant","Variant_cases","Sample_size")]
colnames(nl_baseline)[3] = "count"
colnames(nl_baseline)[4] = "total"

ag = data.frame(aggregate(nl_baseline$count, by=list(collection_date=nl_baseline$collection_date), FUN=sum)) # nr of selected variants
colnames(ag)[2] = "nVOCandVOIs"
ag$total = data.frame(aggregate(nl_baseline$total, by=list(collection_date=nl_baseline$collection_date), FUN=mean))$x # total sequenced
ag$other = ag$total-ag$nVOCandVOIs

nl_baseline = rbind(nl_baseline, data.frame(collection_date=ag$collection_date,
                                            variant="other",
                                            count=ag$other,
                                            total=ag$total
))

nl_baseline$prop = nl_baseline$count/nl_baseline$total

levels_VARIANTS = c(selected_variants,"other")
colours_VARIANTS = c("#0085FF","#9A9D00","cyan3",muted("magenta"),"magenta","grey70")

nl_baseline$variant = factor(nl_baseline$variant, levels=levels_VARIANTS)
nl_baseline$collection_date_num = as.numeric(nl_baseline$collection_date)

range(nl_baseline$collection_date) # "2020-12-03" "2021-06-03"


# Muller plot raw data
muller_nl_raw = ggplot(data=nl_baseline, 
                       aes(x=collection_date, 
                           y=count, fill=variant, group=variant)) +
  # facet_wrap(~PROVINCE) +
  geom_area(aes(fill=variant), position = position_fill(reverse = FALSE)) +
  theme_hc() +
  scale_fill_manual("", values=colours_VARIANTS) +
  scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01")),
                     labels=substring(months(as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01"))),1,1),
                     limits=as.Date(c("2020-12-01",NA)), expand=c(0,0)) +
  ylab("Share") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0)) +
  theme(legend.position = "right") +
  ggtitle("Spread of SARS-CoV2 variants of concern in Netherlands\n(baseline surveillance))")
muller_nl_raw

ggsave("muller plot_netherlands_raw data.png", width=12, height=8)


# multinomial spline fit on share of each variant
# to be able to estimate growth rate advantage of each type compared to given type

set.seed(1)
nl_baseline$variant2 = relevel(nl_baseline$variant, ref="B.1.1.7 (alpha)") # in fits we recode B.1.1.7 as reference strain
nl_seq_mfit0 = nnet::multinom(variant2 ~ collection_date_num, weights=count, data=nl_baseline, maxit=1000)
BIC(nl_seq_mfit0)
summary(nl_seq_mfit0)

# growth rate advantage per day compared to UK type B.1.1.7
delta_r = data.frame(confint(emtrends(nl_seq_mfit0, trt.vs.ctrl ~ variant2|1, 
                                      var="collection_date_num",  mode="latent",
                                      at=list(collection_date_num=today_num)), 
                             adjust="none", df=NA)$contrasts)[,-c(3,4)]
rownames(delta_r) = delta_r[,"contrast"]
delta_r = delta_r[,-1]
delta_r
#                                          estimate     asymp.LCL     asymp.UCL
# B.1.351 (beta) - B.1.1.7 (alpha)    -2.188172e-02 -2.188683e-02 -0.0218766089
# P.1 (gamma) - B.1.1.7 (alpha)        1.233544e-02  1.232947e-02  0.0123414043
# B.1.617.1 (kappa) - B.1.1.7 (alpha) -4.351620e-06 -6.459914e-05  0.0000558959
# B.1.617.2 (delta) - B.1.1.7 (alpha)  7.183150e-02  7.181682e-02  0.0718461859
# other - B.1.1.7 (alpha)             -7.234707e-02 -7.234948e-02 -0.0723446497

# pairwise contrasts in growth rate today (no Tukey correction applied)
emtrends(nl_seq_mfit0, revpairwise ~ variant2|1, 
         var="collection_date_num",  mode="latent",
         at=list(collection_date_num=today_num), 
         df=NA, adjust="none")$contrasts
# contrast                                 estimate          SE df z.ratio    p.value
# B.1.351 (beta) - B.1.1.7 (alpha)      -0.02188172 2.60701e-06 NA  -8393.430 <.0001 
# P.1 (gamma) - B.1.1.7 (alpha)          0.01233544 3.04553e-06 NA   4050.340 <.0001 
# P.1 (gamma) - B.1.351 (beta)           0.03421715 3.98407e-06 NA   8588.500 <.0001 
# B.1.617.1 (kappa) - B.1.1.7 (alpha)   -0.00000435 3.07391e-05 NA     -0.140 0.8874 
# B.1.617.1 (kappa) - B.1.351 (beta)     0.02187737 3.08453e-05 NA    709.260 <.0001 
# B.1.617.1 (kappa) - P.1 (gamma)       -0.01233979 3.08854e-05 NA   -399.530 <.0001 
# B.1.617.2 (delta) - B.1.1.7 (alpha)    0.07183150 7.49216e-06 NA   9587.560 <.0001 
# B.1.617.2 (delta) - B.1.351 (beta)     0.09371322 7.92696e-06 NA  11822.090 <.0001 
# B.1.617.2 (delta) - P.1 (gamma)        0.05949607 8.06301e-06 NA   7378.890 <.0001 
# B.1.617.2 (delta) - B.1.617.1 (kappa)  0.07183585 3.16349e-05 NA   2270.780 <.0001 
# other - B.1.1.7 (alpha)               -0.07234707 1.23252e-06 NA -58698.270 <.0001 
# other - B.1.351 (beta)                -0.05046535 2.77534e-06 NA -18183.460 <.0001 
# other - P.1 (gamma)                   -0.08468250 3.26374e-06 NA -25946.500 <.0001 
# other - B.1.617.1 (kappa)             -0.07234271 3.07596e-05 NA  -2351.870 <.0001 
# other - B.1.617.2 (delta)             -0.14417857 7.59196e-06 NA -18990.970 <.0001 

# pairwise contrasts in growth rate today with confidence intervals:
confint(emtrends(nl_seq_mfit0, revpairwise ~ variant2|1, 
                 var="collection_date_num",  mode="latent",
                 at=list(collection_date_num=today_num), 
                 df=NA, adjust="none"))$contrasts
# contrast                                 estimate          SE df  asymp.LCL  asymp.UCL
# B.1.351 (beta) - B.1.1.7 (alpha)      -0.02188172 2.60701e-06 NA -0.0218868 -0.0218766
# P.1 (gamma) - B.1.1.7 (alpha)          0.01233544 3.04553e-06 NA  0.0123295  0.0123414
# P.1 (gamma) - B.1.351 (beta)           0.03421715 3.98407e-06 NA  0.0342093  0.0342250
# B.1.617.1 (kappa) - B.1.1.7 (alpha)   -0.00000435 3.07391e-05 NA -0.0000646  0.0000559
# B.1.617.1 (kappa) - B.1.351 (beta)     0.02187737 3.08453e-05 NA  0.0218169  0.0219378
# B.1.617.1 (kappa) - P.1 (gamma)       -0.01233979 3.08854e-05 NA -0.0124003 -0.0122793
# B.1.617.2 (delta) - B.1.1.7 (alpha)    0.07183150 7.49216e-06 NA  0.0718168  0.0718462
# B.1.617.2 (delta) - B.1.351 (beta)     0.09371322 7.92696e-06 NA  0.0936977  0.0937288
# B.1.617.2 (delta) - P.1 (gamma)        0.05949607 8.06301e-06 NA  0.0594803  0.0595119
# B.1.617.2 (delta) - B.1.617.1 (kappa)  0.07183585 3.16349e-05 NA  0.0717739  0.0718979
# other - B.1.1.7 (alpha)               -0.07234707 1.23252e-06 NA -0.0723495 -0.0723446
# other - B.1.351 (beta)                -0.05046535 2.77534e-06 NA -0.0504708 -0.0504599
# other - P.1 (gamma)                   -0.08468250 3.26374e-06 NA -0.0846889 -0.0846761
# other - B.1.617.1 (kappa)             -0.07234271 3.07596e-05 NA -0.0724030 -0.0722824
# other - B.1.617.2 (delta)             -0.14417857 7.59196e-06 NA -0.1441935 -0.1441637


# implied increase in infectiousness (due to combination of increased transmissibility and/or immune escape)
# assuming generation time of 4.7 days (Nishiura et al. 2020)
# delta has a 54% [45-64%] increased infectiousness compared to alpha
exp(delta_r*4.7) 
#                                      estimate asymp.LCL asymp.UCL
# B.1.351 (beta) - B.1.1.7 (alpha)    0.9022676 0.9022460 0.9022893
# P.1 (gamma) - B.1.1.7 (alpha)       1.0596901 1.0596604 1.0597199
# B.1.617.1 (kappa) - B.1.1.7 (alpha) 0.9999795 0.9996964 1.0002627
# B.1.617.2 (delta) - B.1.1.7 (alpha) 1.4015911 1.4014943 1.4016878
# other - B.1.1.7 (alpha)             0.7117481 0.7117400 0.7117562


# # PS: mblogit fit would also be possible & would take into account overdispersion
# nl_baseline_long$obs = factor(1:nrow(nl_baseline_long))
# nl_seq_mblogitfit = mblogit(variant ~ scale(collection_date_num, center=TRUE, scale=FALSE),
#                             # random = ~ 1|obs,
#                             weights = count, data=nl_baseline_long, 
#                             subset=nl_baseline_long$variant!="all VOCs",
#                             dispersion = FALSE)
# dispersion(mblogit(variant ~ scale(collection_date_num, center=TRUE, scale=FALSE),
#                    # random = ~ 1|obs,
#                    weights = count, data=nl_baseline_long,
#                    subset = nl_baseline_long$variant=="wild type"|nl_baseline_long$variant=="all VOCs",
#                    dispersion = TRUE), method="Afroz") # dispersion coefficient = 3.2


# plot multinomial model fit ####

# library(effects)
# plot(Effect("collection_date_num",nl_seq_mfit0), style="stacked")

date.from = min(nl_baseline$collection_date_num) 
date.to = as.numeric(as.Date("2021-07-31")) 

nl_seq_mfit0_preds = data.frame(emmeans(nl_seq_mfit0, ~ variant2+collection_date_num, at=list(collection_date_num=seq(date.from, date.to)), mode="prob", df=NA))
nl_seq_mfit0_preds$collection_date = as.Date(nl_seq_mfit0_preds$collection_date_num, origin="1970-01-01")
nl_seq_mfit0_preds$variant = factor(nl_seq_mfit0_preds$variant2, levels=levels_VARIANTS)

muller_nl_seq_mfit0 = ggplot(data=nl_seq_mfit0_preds, 
                             aes(x=collection_date, y=prob, group=variant)) + 
  # facet_wrap(~LABORATORY) +
  geom_area(aes(lwd=I(1.2), colour=NULL, fill=variant), position="stack") +
  annotate("rect", xmin=max(nl_baseline$collection_date)+1, 
           xmax=as.Date("2021-07-31"), ymin=0, ymax=1, alpha=0.3, fill="white") + # extrapolated part
  scale_fill_manual("", values=colours_VARIANTS) +
  scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01")),
                     labels=substring(months(as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01"))),1,1),
                     limits=as.Date(c("2020-12-01","2021-07-31")), expand=c(0,0)) +
  # guides(color = guide_legend(reverse=F, nrow=1, byrow=T), fill = guide_legend(reverse=F, nrow=1, byrow=T)) +
  theme_hc() + theme(legend.position="right", 
                     axis.title.x=element_blank()) + 
  # labs(title = "MAIN SARS-CoV2 VARIANT LINEAGES IN THE UK") +
  ylab("Share") +
  ggtitle("Spread of SARS-CoV2 variants of concern in Netherlands\n(baseline surveillance)")
muller_nl_seq_mfit0

ggsave("muller plot_netherlands_multinomial fit.png", width=12, height=8)

library(ggpubr)
ggarrange(muller_nl_raw+coord_cartesian(xlim=c(as.Date("2020-12-01"),as.Date(date.to, origin="1970-01-01")))+
            theme(legend.background = element_rect(fill = alpha("white", 0)),
                  legend.key = element_rect(fill = alpha("white", 0)),
                  legend.text=element_text(color = "white")) +
            guides(colour = guide_legend(override.aes = list(alpha = 0)),
                   fill = guide_legend(override.aes = list(alpha = 0))), 
          muller_nl_seq_mfit0+ggtitle("Multinomial fit"), ncol=1)

ggsave("muller plot_netherlands_raw data plus multinomial fit multipanel.png", width=12, height=10)
ggsave("muller plot_netherlands_raw data plus multinomial fit multipanel.pdf", width=12, height=10)


# PLOT MODEL FIT WITH DATA & CONFIDENCE INTERVALS

# on response scale:
plot_multinom_response = qplot(data=nl_seq_mfit0_preds, 
                               x=collection_date, y=100*prob, geom="blank") +
  geom_ribbon(aes(y=100*prob, ymin=100*asymp.LCL, ymax=100*asymp.UCL, colour=NULL,
                  fill=variant
  ), alpha=I(0.3)) +
  geom_line(aes(y=100*prob,
                colour=variant
  ), alpha=I(1)) +
  ylab("Share among newly diagnosed infections (%)") +
  theme_hc() + xlab("") +
  ggtitle("Spread of SARS-CoV2 variants of concern in Netherlands\n(baseline surveillance, sequencing+VOC PCRs)") +
  # scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01")),
  #                   labels=c("M","A","M","J","J","A","S","O","N","D","J","F","M")) +
  # scale_y_continuous( trans="logit", breaks=c(10^seq(-5,0),0.5,0.9,0.99,0.999),
  #                     labels = c("0.001","0.01","0.1","1","10","100","50","90","99","99.9")) +
  scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01")),
                     labels=substring(months(as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01"))),1,1),
                     limits=as.Date(c("2020-12-01","2021-07-31")), expand=c(0,0)) +
  coord_cartesian(xlim=c(min(nl_baseline$collection_date), as.Date("2021-07-31")),
                  # xlim=c(as.Date("2020-07-01"),as.Date("2021-01-31")),
                  ylim=c(0,100), expand=c(0,0)) +
  # scale_color_discrete("", h=c(0, 280), c=200) +
  # scale_fill_discrete("", h=c(0, 280), c=200) +
  scale_fill_manual("variant", values=colours_VARIANTS) + # c("red","blue","green3","magenta","black")
  scale_colour_manual("variant", values=colours_VARIANTS) + # c("red","blue","green3","magenta","black")
  geom_point(data=nl_baseline,
             aes(x=collection_date, y=100*prop, size=total,
                 colour=variant
             ),
             alpha=I(1)) +
  scale_size_continuous("total n", trans="sqrt",
                        range=c(0.01, 6), limits=c(1,10^4), breaks=c(10,100,1000)) +
  # guides(fill=FALSE) +
  # guides(colour=FALSE) +
  theme(legend.position = "right") +
  xlab("Collection date")
plot_multinom_response

ggsave("netherlands_baseline_surveillance_multinomial fits_response scale.png", width=12, height=10)


# on logit scale:

nl_seq_mfit0_preds2 = nl_seq_mfit0_preds
ymin = 0.001
ymax = 0.990001
nl_seq_mfit0_preds2$asymp.LCL[nl_seq_mfit0_preds2$asymp.LCL<ymin] = ymin
nl_seq_mfit0_preds2$asymp.UCL[nl_seq_mfit0_preds2$asymp.UCL<ymin] = ymin
nl_seq_mfit0_preds2$asymp.UCL[nl_seq_mfit0_preds2$asymp.UCL>ymax] = ymax
nl_seq_mfit0_preds2$prob[nl_seq_mfit0_preds2$prob<ymin] = ymin

plot_multinom = qplot(data=nl_seq_mfit0_preds2[nl_seq_mfit0_preds2$variant!="all VOCs",], x=collection_date, y=prob, geom="blank") +
  geom_ribbon(aes(y=prob, ymin=asymp.LCL, ymax=asymp.UCL, colour=NULL,
                  fill=variant
  ), alpha=I(0.3)) +
  geom_line(aes(y=prob,
                colour=variant
  ), alpha=I(1)) +
  ylab("Share among newly diagnosed infections (%)") +
  theme_hc() + xlab("") +
  ggtitle("Spread of SARS-CoV2 variants of concern in Netherlands\n(baseline surveillance)") +
  # scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01")),
  #                   labels=c("M","A","M","J","J","A","S","O","N","D","J","F","M")) +
  scale_y_continuous( trans="logit", breaks=c(10^seq(-5,0),0.5,0.9,0.99,0.999),
                      labels = c("0.001","0.01","0.1","1","10","100","50","90","99","99.9")) +
  scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01")),
                     labels=substring(months(as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01"))),1,1),
                     limits=as.Date(c("2020-12-01","2021-07-31")), expand=c(0,0)) +
  # scale_color_discrete("", h=c(0, 280), c=200) +
  # scale_fill_discrete("", h=c(0, 280), c=200) +
  scale_fill_manual("variant", values=colours_VARIANTS) + # c("red","blue","green3","magenta","black")
  scale_colour_manual("variant", values=colours_VARIANTS) + # c("red","blue","green3","magenta","black")
  geom_point(data=nl_baseline,
             aes(x=collection_date, y=prop, size=total,
                 colour=variant
             ),
             alpha=I(1)) +
  scale_size_continuous("total n", trans="sqrt",
                        range=c(0.01, 6), limits=c(10,10^4), breaks=c(10,100,1000)) +
  # guides(fill=FALSE) +
  # guides(colour=FALSE) +
  theme(legend.position = "right") +
  xlab("Collection date") +
  coord_cartesian(xlim=c(min(nl_baseline$collection_date), as.Date("2021-07-31")),
                  # xlim=c(as.Date("2020-07-01"),as.Date("2021-01-31")),
                  ylim=c(0.001, 0.9900001), expand=c(0,0))
plot_multinom


ggsave("netherlands_baseline_surveillance_multinomial fits_link scale.png", width=12, height=10)


# estimated share of different variants of concern among lab diagnoses today
nl_seq_mfit0_preds[as.character(nl_seq_mfit0_preds$collection_date)==as.character(today),]
#               variant2 collection_date_num         prob           SE df     asymp.LCL    asymp.UCL collection_date           variant
# 1225   B.1.1.7 (alpha)             18803.5 0.8263906407 4.859598e-02 NA  7.311443e-01 9.216370e-01      2021-06-25   B.1.1.7 (alpha)
# 1226    B.1.351 (beta)             18803.5 0.0022110725 3.766532e-04 NA  1.472846e-03 2.949299e-03      2021-06-25    B.1.351 (beta)
# 1227       P.1 (gamma)             18803.5 0.0297013947 4.343807e-03 NA  2.118769e-02 3.821510e-02      2021-06-25       P.1 (gamma)
# 1228 B.1.617.1 (kappa)             18803.5 0.0001332019 1.983354e-04 NA -2.555284e-04 5.219321e-04      2021-06-25 B.1.617.1 (kappa)
# 1229 B.1.617.2 (delta)             18803.5 0.1415150819 5.032479e-02 NA  4.288030e-02 2.401499e-01      2021-06-25 B.1.617.2 (delta)
# 1230             other             18803.5 0.0000486083 6.944050e-06 NA  3.499821e-05 6.221839e-05      2021-06-25             other

# estimated share of different variants of concern among new infections today (assuming 1 week between infection & diagnosis)
nl_seq_mfit0_preds[as.character(nl_seq_mfit0_preds$collection_date)==as.character(today+7),]
# 1267   B.1.1.7 (alpha)             18810.5 7.366091e-01 8.392591e-02 NA  5.721173e-01 9.011008e-01      2021-07-02   B.1.1.7 (alpha)
# 1268    B.1.351 (beta)             18810.5 1.688067e-03 3.454789e-04 NA  1.010941e-03 2.365193e-03      2021-07-02    B.1.351 (beta)
# 1269       P.1 (gamma)             18810.5 2.902183e-02 5.365833e-03 NA  1.850499e-02 3.953867e-02      2021-07-02       P.1 (gamma)
# 1270 B.1.617.1 (kappa)             18810.5 1.206469e-04 1.941868e-04 NA -2.599521e-04 5.012460e-04      2021-07-02 B.1.617.1 (kappa)
# 1271 B.1.617.2 (delta)             18810.5 2.325342e-01 8.734635e-02 NA  6.133850e-02 4.037299e-01      2021-07-02 B.1.617.2 (delta)
# 1272             other             18810.5 2.616771e-05 4.668626e-06 NA  1.701737e-05 3.531805e-05      2021-07-02             other



# estimated date that B.1.617.2 would make out >50% of all lab diagnoses: "2021-07-16" ["2021-07-06"-30/7/2021] 95% CLs (7 days earlier for infections)
nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","collection_date"][which(nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","prob"] >= 0.5)[1]]
nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","collection_date"][which(nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","asymp.LCL"] >= 0.5)[1]]
nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","collection_date"][which(nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","asymp.UCL"] >= 0.5)[1]]


# CALCULATE EFFECTIVE REPRODUCTION BASED ON CASES & EFFECTIVE REPRODUCTION NUMBER OF VARIANTS THROUGH TIME ####

