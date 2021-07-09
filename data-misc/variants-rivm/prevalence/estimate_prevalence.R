# ANALYSIS OF GROWTH ADVANTAGE OF DIFFERENT SARS-CoV2 VARIANTS OF CONCERN IN THE NETHERLANDS ####
# Tom Wenseleers

# Data: baseline surveillance whole genome sequencing RIVM, https://data.rivm.nl/covid-19/COVID-19_varianten.csv / https://raw.githubusercontent.com/mzelst/covid-19/master/data-misc/variants-rivm/prevalence_variants.csv

# Tom Wenseleers, last update 4 July 2021, Re values updated 7 July 2021

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
# library(devtools)
# devtools::install_github("tomwenseleers/export")
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
# remotes::install_github("rvlenth/emmeans", dependencies = TRUE, force = TRUE)
library(emmeans)
library(broom)
library(nnet)
# devtools::install_github("melff/mclogit",subdir="pkg") # install latest development version of mclogit, to add emmeans support
library(mclogit)
library(lubridate)

setwd("data-misc/variants-rivm/prevalence")


# filedate = as.Date(gsub("_","-",dat)) # file date
# filedate_num = as.numeric(filedate)
# today = as.Date(Sys.time()) # we use the file date version as our definition of "today"
today = as.Date("2021-07-07")
today_num = as.numeric(today)

selected_variants = c("B.1.1.7 (alpha)", "B.1.351 (beta)", "P.1 (gamma)", # B.1.617.1 (kappa)", 
                      "B.1.617.2 (delta)")
levels_VARIANTS = c(selected_variants,"other")
colours_VARIANTS = c("#0085FF","#9A9D00","cyan3", # muted("magenta"),
                     "magenta","grey70")

set_sum_contrasts() # we use effect coding for all models


# 1. ASSESSMENT OF GROWTH RATE ADVANTAGES OF VOCs B.1.1.7 (alpha), B.1.351 (beta), P.1 (gamma), B.1.617.1 (kappa) & B.1.617.2 (delta)
# IN THE NETHERLANDS BASED ON BASELINE SURVEILLANCE SEQUENCING & VOC PCR DATA ####
# (baseline surveillance, i.e. randomly sampled, excluding travellers & surge testing)


# official RIVM csv
nl_baseline = read.csv("https://data.rivm.nl/covid-19/COVID-19_varianten.csv", sep=";")
nl_baseline$collection_date = as.Date(nl_baseline$Date_of_statistics_week_start)+3.5 # we use week midpoint
nl_baseline$variant = paste0(nl_baseline$Variant_code, " (", tolower(nl_baseline$Variant_name),")")
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


# using Marino van Zelst's copy of RIVM variant data
# nl_baseline = read.csv("https://raw.githubusercontent.com/mzelst/covid-19/master/data-misc/variants-rivm/prevalence_variants.csv")
# nl_baseline = nl_baseline[,c("Week","Jaar","Aantal_monsters","Britse_variant","ZuidAfrikaanse_variant","Braziliaanse_variant_P1","Indiase_Variant_B1.167.1","Indiase_Variant_B1.167.2")]
# # for collection date we use the week midpoint:
# collection_date = c(lubridate::ymd( paste0(nl_baseline$Jaar[nl_baseline$Jaar==2020],"-01-01") ) + lubridate::weeks( nl_baseline$Week[nl_baseline$Jaar==2020] - 1 ) - 2, 
#                     lubridate::ymd( paste0(nl_baseline$Jaar[nl_baseline$Jaar==2021],"-01-01") ) + lubridate::weeks( nl_baseline$Week[nl_baseline$Jaar==2021] - 1 )+3) + 3.5
# nl_baseline = data.frame(collection_date=collection_date,
#                          nl_baseline[,-c(1:2)])
# colnames(nl_baseline) = c("collection_date", "total", "B.1.1.7 (alpha)", "B.1.351 (beta)", "P.1 (gamma)", "B.1.617.1 (kappa)", "B.1.617.2 (delta)")
# nl_baseline$other = nl_baseline$total-rowSums(nl_baseline[,-c(1:2)])
# nl_baseline = gather(nl_baseline[,c("collection_date", # convert to long format
#                                          "B.1.1.7 (alpha)",
#                                          "B.1.351 (beta)",
#                                          "P.1 (gamma)",
#                                          "B.1.617.1 (kappa)",
#                                          "B.1.617.2 (delta)",
#                                          "other",
#                                          "total")], 
#                           variant, count, c("B.1.1.7 (alpha)",
#                                             "B.1.351 (beta)",
#                                             "P.1 (gamma)",
#                                             "B.1.617.1 (kappa)",
#                                             "B.1.617.2 (delta)",
#                                             "other"), factor_key=TRUE)


nl_baseline$variant = factor(nl_baseline$variant, levels=levels_VARIANTS)
nl_baseline$prop = nl_baseline$count/nl_baseline$total
nl_baseline$collection_date_num = as.numeric(nl_baseline$collection_date)

range(nl_baseline$collection_date) # "2020-12-03" "2021-06-17"


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
  xlab("Date of diagnosis") +
  theme(plot.title = element_text(hjust = 0)) +
  theme(legend.position = "right") +
  ggtitle("Spread of SARS-CoV2 variants of concern in the Netherlands\n(baseline surveillance, data RIVM)")
muller_nl_raw

ggsave("muller plot_netherlands_raw data.png", width=12, height=8)


# multinomial spline fit on share of each variant
# to be able to estimate growth rate advantage of each type compared to given type

set.seed(1)
nl_baseline$variant2 = relevel(nl_baseline$variant, ref="B.1.1.7 (alpha)") # in fits we recode B.1.1.7 as reference strain
nl_seq_mfit0 = nnet::multinom(variant2 ~ ns(collection_date_num, df=3), weights=count, data=nl_baseline, maxit=1000)
BIC(nl_seq_mfit0) # 3 df gave best BIC
summary(nl_seq_mfit0)

# growth rate advantage per day compared to UK type B.1.1.7
delta_r = data.frame(confint(emtrends(nl_seq_mfit0, trt.vs.ctrl ~ variant2|1, 
                                      var="collection_date_num",  mode="latent",
                                      at=list(collection_date_num=today_num)), 
                             adjust="none", df=NA)$contrasts)[,-c(3,4)]
rownames(delta_r) = delta_r[,"contrast"]
delta_r = delta_r[,-1]
delta_r
# estimate    asymp.LCL   asymp.UCL
# B.1.351 (beta) - B.1.1.7 (alpha)    -0.061864159 -0.081939474 -0.04178884
# P.1 (gamma) - B.1.1.7 (alpha)        0.002699051 -0.008761275  0.01415938
# B.1.617.2 (delta) - B.1.1.7 (alpha)  0.116320458  0.099636445  0.13300447
# other - B.1.1.7 (alpha)              0.040707562  0.033323591  0.04809153

# pairwise contrasts in growth rate today (no Tukey correction applied)
emtrends(nl_seq_mfit0, revpairwise ~ variant2|1, 
         var="collection_date_num",  mode="latent",
         at=list(collection_date_num=today_num), 
         df=NA, adjust="none")$contrasts
# contrast                            estimate      SE df z.ratio p.value
# B.1.351 (beta) - B.1.1.7 (alpha)     -0.0619 0.01024 NA -6.040  <.0001 
# P.1 (gamma) - B.1.1.7 (alpha)         0.0027 0.00585 NA  0.462  0.6444 
# P.1 (gamma) - B.1.351 (beta)          0.0646 0.01179 NA  5.477  <.0001 
# B.1.617.2 (delta) - B.1.1.7 (alpha)   0.1163 0.00851 NA 13.665  <.0001 
# B.1.617.2 (delta) - B.1.351 (beta)    0.1782 0.01332 NA 13.373  <.0001 
# B.1.617.2 (delta) - P.1 (gamma)       0.1136 0.01026 NA 11.073  <.0001 
# other - B.1.1.7 (alpha)               0.0407 0.00377 NA 10.805  <.0001 
# other - B.1.351 (beta)                0.1026 0.01079 NA  9.508  <.0001 
# other - P.1 (gamma)                   0.0380 0.00693 NA  5.484  <.0001 
# other - B.1.617.2 (delta)            -0.0756 0.00926 NA -8.166  <.0001 
# 
# Degrees-of-freedom method: user-specified 


# pairwise contrasts in growth rate today with confidence intervals:
confint(emtrends(nl_seq_mfit0, revpairwise ~ variant2|1, 
                 var="collection_date_num",  mode="latent",
                 at=list(collection_date_num=today_num), 
                 df=NA, adjust="none"))$contrasts
# contrast                            estimate      SE df asymp.LCL asymp.UCL
# B.1.351 (beta) - B.1.1.7 (alpha)     -0.0619 0.01024 NA  -0.08194   -0.0418
# P.1 (gamma) - B.1.1.7 (alpha)         0.0027 0.00585 NA  -0.00876    0.0142
# P.1 (gamma) - B.1.351 (beta)          0.0646 0.01179 NA   0.04146    0.0877
# B.1.617.2 (delta) - B.1.1.7 (alpha)   0.1163 0.00851 NA   0.09964    0.1330
# B.1.617.2 (delta) - B.1.351 (beta)    0.1782 0.01332 NA   0.15207    0.2043
# B.1.617.2 (delta) - P.1 (gamma)       0.1136 0.01026 NA   0.09351    0.1337
# other - B.1.1.7 (alpha)               0.0407 0.00377 NA   0.03332    0.0481
# other - B.1.351 (beta)                0.1026 0.01079 NA   0.08143    0.1237
# other - P.1 (gamma)                   0.0380 0.00693 NA   0.02442    0.0516
# other - B.1.617.2 (delta)            -0.0756 0.00926 NA  -0.09376   -0.0575
# 
# Degrees-of-freedom method: user-specified 
# Confidence level used: 0.95 



# implied increase in infectiousness (due to combination of increased transmissibility and/or immune escape)
# assuming generation time of 4.7 days (Nishiura et al. 2020)
# delta has a 73% [60-87%] increased infectiousness compared to alpha
exp(delta_r*4.7) 
# estimate asymp.LCL asymp.UCL
# B.1.351 (beta) - B.1.1.7 (alpha)    0.7476939 0.6803720 0.8216773
# P.1 (gamma) - B.1.1.7 (alpha)       1.0127663 0.9596583 1.0688134
# B.1.617.2 (delta) - B.1.1.7 (alpha) 1.7275533 1.5972626 1.8684721
# other - B.1.1.7 (alpha)             1.2108536 1.1695521 1.2536135


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
  theme_hc() + theme(legend.position="right") + 
  # labs(title = "MAIN SARS-CoV2 VARIANT LINEAGES IN THE UK") +
  ylab("Share") + xlab("Date of diagnosis") +
  ggtitle("Spread of SARS-CoV2 variants of concern in the Netherlands\n(baseline surveillance, data RIVM)")
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
  theme_hc() + xlab("Date of diagnosis") +
  ggtitle("Spread of SARS-CoV2 variants of concern in the Netherlands\n(baseline surveillance, data RIVM)") +
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
  theme(legend.position = "right") 
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
  theme_hc() + xlab("Date of diagnosis") +
  ggtitle("Spread of SARS-CoV2 variants of concern in the Netherlands\n(baseline surveillance, data RIVM)") +
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
  coord_cartesian(xlim=c(min(nl_baseline$collection_date), as.Date("2021-07-31")),
                  # xlim=c(as.Date("2020-07-01"),as.Date("2021-01-31")),
                  ylim=c(0.001, 0.9900001), expand=c(0,0))
plot_multinom


ggsave("netherlands_baseline_surveillance_multinomial fits_link scale.png", width=12, height=10)


# estimated share of different variants of concern among lab diagnoses today
nl_seq_mfit0_preds[as.character(nl_seq_mfit0_preds$collection_date)==as.character(today),]
# variant2 collection_date_num         prob           SE df     asymp.LCL    asymp.UCL collection_date           variant
# 1081   B.1.1.7 (alpha)             18815.5 3.055692e-01 5.353719e-02 NA  2.006383e-01 0.4105001913      2021-07-07   B.1.1.7 (alpha)
# 1082    B.1.351 (beta)             18815.5 4.256939e-05 2.944302e-05 NA -1.513787e-05 0.0001002767      2021-07-07    B.1.351 (beta)
# 1083       P.1 (gamma)             18815.5 6.730517e-03 2.205889e-03 NA  2.407054e-03 0.0110539800      2021-07-07       P.1 (gamma)
# 1084 B.1.617.2 (delta)             18815.5 6.686882e-01 5.796757e-02 NA  5.550739e-01 0.7823025822      2021-07-07 B.1.617.2 (delta)
# 1085             other             18815.5 1.896944e-02 5.271079e-03 NA  8.638315e-03 0.0293005644      2021-07-07             other

# estimated share of different variants of concern among new infections today (assuming 1 week between infection & diagnosis)
nl_seq_mfit0_preds[as.character(nl_seq_mfit0_preds$collection_date)==as.character(today+7),]
# variant2 collection_date_num         prob           SE df     asymp.LCL    asymp.UCL collection_date           variant
# 1116   B.1.1.7 (alpha)             18822.5 1.654211e-01 0.043212100 NA  8.072691e-02 2.501152e-01      2021-07-14   B.1.1.7 (alpha)
# 1117    B.1.351 (beta)             18822.5 1.494541e-05 0.000011727 NA -8.039098e-06 3.792991e-05      2021-07-14    B.1.351 (beta)
# 1118       P.1 (gamma)             18822.5 3.713085e-03 0.001521792 NA  7.304283e-04 6.695742e-03      2021-07-14       P.1 (gamma)
# 1119 B.1.617.2 (delta)             18822.5 8.171960e-01 0.047739513 NA  7.236283e-01 9.107637e-01      2021-07-14 B.1.617.2 (delta)
# 1120             other             18822.5 1.365492e-02 0.004842951 NA  4.162912e-03 2.314693e-02      2021-07-14             other



# estimated date that B.1.617.2 would make out >50% of all lab diagnoses: "2021-07-01" [2021-06-28-2021-07-05] 95% CLs (7 days earlier for infections)
nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","collection_date"][which(nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","prob"] >= 0.5)[1]]
nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","collection_date"][which(nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","asymp.LCL"] >= 0.5)[1]]
nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","collection_date"][which(nl_seq_mfit0_preds[nl_seq_mfit0_preds$variant=="B.1.617.2 (delta)","asymp.UCL"] >= 0.5)[1]]


# PLOTS OF NEW CASES PER DAY BY VARIANT & EFFECTIVE REPRODUCTION NUMBER BY VARIANT THROUGH TIME ####

# load case data

library(covidregionaldata)
library(dplyr)
library(ggplot2)
library(scales)  

cases_tot = as.data.frame(get_national_data(countries = "Netherlands"))
cases_tot = cases_tot[cases_tot$date>=as.Date("2020-08-01"),]
cases_tot$DATE_NUM = as.numeric(cases_tot$date)
# cases_tot$BANKHOLIDAY = bankholiday(cases_tot$date)
cases_tot$WEEKDAY = weekdays(cases_tot$date)
# cases_tot = cases_tot[cases_tot$date<=(max(cases_tot$date)-3),] # cut off data from last 3 days (incomplete)
range(cases_tot$date) # "2020-08-01" "2021-07-07"

# smooth out weekday effects in case nrs using GAM (if testing data is available one could correct for testing intensity as well)
fit_cases = gam(cases_new ~ s(DATE_NUM, bs="cs", k=30, fx=F) + 
                  WEEKDAY, # + 
                # BANKHOLIDAY,
                # s(TESTS_ALL, bs="cs", k=8, fx=F),
                family=poisson(log), data=cases_tot,
) 
BIC(fit_cases)

# STACKED AREA CHART OF NEW CASES BY VARIANT (MULTINOMIAL FIT MAPPED ONTO CASE DATA) ####

nl_seq_mfit0_preds$totcases = cases_tot$cases_new[match(round(nl_seq_mfit0_preds$collection_date_num),cases_tot$DATE_NUM)]
nl_seq_mfit0_preds$cases = nl_seq_mfit0_preds$totcases * nl_seq_mfit0_preds$prob
nl_seq_mfit0_preds$cases[nl_seq_mfit0_preds$cases<=0.001] = NA
cases_emmeans = as.data.frame(emmeans(fit_cases, ~ DATE_NUM, at=list(DATE_NUM=seq(date.from, date.to, by=0.5), BANHOLIDAY="no"), type="response"))
nl_seq_mfit0_preds$smoothed_totcases = cases_emmeans$rate[match(nl_seq_mfit0_preds$collection_date_num,cases_emmeans$DATE_NUM)]
nl_seq_mfit0_preds$smoothed_cases = nl_seq_mfit0_preds$smoothed_totcases * nl_seq_mfit0_preds$prob
nl_seq_mfit0_preds$smoothed_cases[nl_seq_mfit0_preds$smoothed_cases<=0.001] = NA

ggplot(data=nl_seq_mfit0_preds, 
       aes(x=collection_date, y=cases, group=variant)) + 
  # facet_wrap(~ REGION, scale="free", ncol=3) +
  geom_area(aes(lwd=I(1.2), colour=NULL, fill=variant, group=variant), position="stack") +
  scale_x_continuous(breaks=as.Date(c("2020-01-01","2020-02-01","2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01")),
                     labels=substring(months(as.Date(c("2020-01-01","2020-02-01","2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01"))),1,1),
                     # limits=c(as.Date("2021-03-01"),max(cases_tot$date)), 
                     expand=c(0,0)) +
  # guides(color = guide_legend(reverse=F, nrow=1, byrow=T), fill = guide_legend(reverse=F, nrow=1, byrow=T)) +
  theme_hc() + theme(legend.position="right") + 
  ylab("New confirmed cases per day") + xlab("Date of diagnosis") +
  ggtitle("NEW CONFIRMED SARS-CoV2 CASES PER DAY BY VARIANT\nIN THE NETHERLANDS\n(case data & multinomial fit to baseline surveillance data RIVM)") +
  scale_fill_manual("variant", values=colours_VARIANTS) +
  scale_colour_manual("variant", values=colours_VARIANTS) +
  coord_cartesian(xlim=c(as.Date("2020-12-01"),NA))

ggsave(file="cases per day_stacked area multinomial fit raw case data.png", width=8, height=6)

ggplot(data=nl_seq_mfit0_preds[nl_seq_mfit0_preds$collection_date<=max(cases_tot$date),], 
       aes(x=collection_date-7, y=smoothed_cases, group=variant)) + 
  # facet_wrap(~ REGION, scale="free", ncol=3) +
  geom_area(aes(lwd=I(1.2), colour=NULL, fill=variant, group=variant), position="stack") +
  scale_x_continuous(breaks=as.Date(c("2020-01-01","2020-02-01","2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01")),
                     labels=substring(months(as.Date(c("2020-01-01","2020-02-01","2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01"))),1,1),
                     # limits=c(as.Date("2021-03-01"),today), 
                     expand=c(0,0)) +
  # guides(color = guide_legend(reverse=F, nrow=1, byrow=T), fill = guide_legend(reverse=F, nrow=1, byrow=T)) +
  theme_hc() + theme(legend.position="right") + 
  ylab("New confirmed cases per day (smoothed)") + xlab("Date of infection") +
  ggtitle("NEW CONFIRMED SARS-CoV2 CASES PER DAY BY VARIANT\nIN THE NETHERLANDS\n(case data & multinomial fit to baseline surveillance data RIVM)") +
  scale_fill_manual("variant", values=colours_VARIANTS) +
  scale_colour_manual("variant", values=colours_VARIANTS) +
  coord_cartesian(xlim=c(as.Date("2020-12-01"),max(cases_tot$date)))

ggsave("cases per day_smoothed_stacked area multinomial fit raw case data.png", width=8, height=6)


# EFFECTIVE REPRODUCTION NUMBER BY VARIANT THROUGH TIME ####

# Function to calculate Re values from intrinsic growth rate
# cf. https://github.com/epiforecasts/EpiNow2/blob/5015e75f7048c2580b2ebe83e46d63124d014861/R/utilities.R#L109
# https://royalsocietypublishing.org/doi/10.1098/rsif.2020.0144
# (assuming gamma distributed gen time)
Re.from.r <- function(r, gamma_mean=4.7, gamma_sd=2.9) { # Nishiura et al. 2020, or use values from Ferretti et al. 2020 (gamma_mean=5.5, gamma_sd=1.8)
  k <- (gamma_sd / gamma_mean)^2
  R <- (1 + k * r * gamma_mean)^(1 / k)
  return(R)
}


# calculate average instantaneous growth rates & 95% CLs using emtrends ####
# based on the slope of the GAM fit on a log link scale
avg_r_cases = as.data.frame(emtrends(fit_cases, ~ DATE_NUM, var="DATE_NUM", 
                                     at=list(DATE_NUM=seq(date.from,
                                                          date.to)
                                     ), # weekday="Wednesday",
                                     type="link"))
colnames(avg_r_cases)[2] = "r"
colnames(avg_r_cases)[5] = "r_LOWER"
colnames(avg_r_cases)[6] = "r_UPPER"
avg_r_cases$DATE = as.Date(avg_r_cases$DATE_NUM, origin="1970-01-01") # -7 TO CALCULATE BACK TO INFECTION DATE
avg_r_cases$Re = Re.from.r(avg_r_cases$r)
avg_r_cases$Re_LOWER = Re.from.r(avg_r_cases$r_LOWER)
avg_r_cases$Re_UPPER = Re.from.r(avg_r_cases$r_UPPER)
avg_r_cases = avg_r_cases[complete.cases(avg_r_cases),]
avg_r_cases[avg_r_cases$DATE==max(avg_r_cases$DATE),] # Re now at 1.863273
qplot(data=avg_r_cases, x=DATE-7, y=Re, ymin=Re_LOWER, ymax=Re_UPPER, geom="ribbon", alpha=I(0.5), fill=I("steelblue")) +
  # facet_wrap(~ REGION) +
  geom_line() + theme_hc() + xlab("Date of infection") +
  scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01")),
                     labels=c("M","A","M","J","J","A","S","O","N","D","J","F","M","A","M","J","J")) +
  # scale_y_continuous(limits=c(1/2, 2), trans="log2") +
  geom_hline(yintercept=1, colour=I("red")) +
  ggtitle("Re IN THE NETHERLANDS AT MOMENT OF INFECTION BASED ON NEW CASES\n(data RIVM)") +
  # labs(tag = tag) +
  # theme(plot.margin = margin(t = 20, r = 10, b = 20, l = 0)) +
  theme(plot.tag.position = "bottomright",
        plot.tag = element_text(vjust = 1, hjust = 1, size=8)) # +
# coord_cartesian(xlim=c(as.Date("2020-01-01"),NA))

# calculate above-average intrinsic growth rates per day of each variant over time based on multinomial fit using emtrends weighted effect contrasts ####
# for best model fit3_sanger_multi
above_avg_r_variants0 = do.call(rbind, lapply(seq(date.from,
                                                  date.to), 
                                              function (dat) { 
                                                wt = as.data.frame(emmeans(nl_seq_mfit0, ~ variant2 , at=list(collection_date_num=dat), type="response"))$prob   # important: these should sum to 1
                                                # wt = rep(1/length(levels_VARIANTS), length(levels_VARIANTS)) # this would give equal weights, equivalent to emmeans:::eff.emmc(levs=levels_LINEAGE2)
                                                cons = lapply(seq_along(wt), function (i) { con = -wt; con[i] = 1 + con[i]; con })
                                                names(cons) = seq_along(cons)
                                                EMT = emtrends(nl_seq_mfit0,  ~ variant2 , by=c("collection_date_num"),
                                                               var="collection_date_num", mode="latent",
                                                               at=list(collection_date_num=dat))
                                                out = as.data.frame(confint(contrast(EMT, cons), adjust="none", df=NA))
                                                # sum(out$estimate*wt) # should sum to zero
                                                return(out) } ))
above_avg_r_variants = above_avg_r_variants0
above_avg_r_variants$contrast = factor(above_avg_r_variants$contrast, 
                                       levels=1:length(levels_VARIANTS), 
                                       labels=levels_VARIANTS)
above_avg_r_variants$variant = above_avg_r_variants$contrast # gsub(" effect|\\(|\\)","",above_avg_r_variants$contrast)
above_avg_r_variants$collection_date = as.Date(above_avg_r_variants$collection_date_num, origin="1970-01-01")
range(above_avg_r_variants$collection_date) # "2020-12-03" "2021-07-30"
above_avg_r_variants$avg_r = avg_r_cases$r[match(above_avg_r_variants$collection_date,
                                                 avg_r_cases$DATE)]  # average growth rate of all lineages calculated from case nrs
above_avg_r_variants$r = above_avg_r_variants$avg_r+above_avg_r_variants$estimate
above_avg_r_variants$r_LOWER = above_avg_r_variants$avg_r+above_avg_r_variants$asymp.LCL
above_avg_r_variants$r_UPPER = above_avg_r_variants$avg_r+above_avg_r_variants$asymp.UCL
above_avg_r_variants$Re = Re.from.r(above_avg_r_variants$r)
above_avg_r_variants$Re_LOWER = Re.from.r(above_avg_r_variants$r_LOWER)
above_avg_r_variants$Re_UPPER = Re.from.r(above_avg_r_variants$r_UPPER)
df = data.frame(contrast=NA,
                collection_date_num=avg_r_cases$DATE_NUM, # -7 to calculate back to time of infection
                # REGION=avg_r_cases$REGION,
                estimate=NA,
                SE=NA,
                df=NA,
                asymp.LCL=NA,
                asymp.UCL=NA,
                # p.value=NA,
                collection_date=avg_r_cases$DATE,
                variant="avg",
                avg_r=avg_r_cases$r,
                r=avg_r_cases$r,
                r_LOWER=avg_r_cases$r_LOWER,
                r_UPPER=avg_r_cases$r_UPPER,
                Re=avg_r_cases$Re,
                Re_LOWER=avg_r_cases$Re_LOWER,
                Re_UPPER=avg_r_cases$Re_UPPER)
# df = df[df$DATE_NUM<=max(above_avg_r_variants$DATE_NUM)&df$DATE_NUM>=(min(above_avg_r_variants$DATE_NUM)+7),]
above_avg_r_variants = rbind(above_avg_r_variants, df)
above_avg_r_variants$variant = factor(above_avg_r_variants$variant, levels=c(levels_VARIANTS,"avg"))
above_avg_r_variants$prob = nl_seq_mfit0_preds$prob[match(interaction(above_avg_r_variants$collection_date_num,
                                                                      above_avg_r_variants$variant),
                                                          interaction(nl_seq_mfit0_preds$collection_date_num,
                                                                      nl_seq_mfit0_preds$variant))]
above_avg_r_variants2 = above_avg_r_variants
ymax = 3
ymin = 1/2
above_avg_r_variants2$Re[above_avg_r_variants2$Re>=ymax] = NA
above_avg_r_variants2$Re[above_avg_r_variants2$Re<=ymin] = NA
above_avg_r_variants2$Re_LOWER[above_avg_r_variants2$Re_LOWER>=ymax] = ymax
above_avg_r_variants2$Re_LOWER[above_avg_r_variants2$Re_LOWER<=ymin] = ymin
above_avg_r_variants2$Re_UPPER[above_avg_r_variants2$Re_UPPER>=ymax] = ymax
above_avg_r_variants2$Re_UPPER[above_avg_r_variants2$Re_UPPER<=ymin] = ymin
above_avg_r_variants2$Re[above_avg_r_variants2$prob<0.01] = NA
above_avg_r_variants2$Re_LOWER[above_avg_r_variants2$prob<0.01] = NA
above_avg_r_variants2$Re_UPPER[above_avg_r_variants2$prob<0.01] = NA
qplot(data=above_avg_r_variants2[!((above_avg_r_variants2$variant %in% c("other"))|above_avg_r_variants2$collection_date>max(cases_tot$DATE)),], 
      x=collection_date-7, # -7 to calculate back to date of infection
      y=Re, ymin=Re_LOWER, ymax=Re_UPPER, geom="ribbon", colour=variant, fill=variant, alpha=I(0.5),
      group=variant, linetype=I(0)) +
  # facet_wrap(~ REGION) +
  # geom_ribbon(aes(fill=variant, colour=variant), alpha=I(0.5))
  geom_line(aes(colour=variant), lwd=I(0.72)) + theme_hc() + xlab("Date of infection") +
  scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01")),
                     labels=c("M","A","M","J","J","A","S","O","N","D","J","F","M","A","M","J","J")) +
  # scale_y_continuous(limits=c(1/ymax,ymax), trans="log2") +
  geom_hline(yintercept=1, colour=I("red")) +
  ggtitle("Re VALUES OF SARS-CoV2 VARIANTS IN THE NETHERLANDS\nAT MOMENT OF INFECTION\n(based on RIVM case data & multinomial fit to\nbaseline surveillance lineage frequencies)") +
  # labs(tag = tag) +
  # theme(plot.margin = margin(t = 20, r = 10, b = 20, l = 0)) +
  theme(plot.tag.position = "bottomright",
        plot.tag = element_text(vjust = 1, hjust = 1, size=8)) +
  coord_cartesian(xlim=c(as.Date("2021-01-01"),max(cases_tot$date))) +
  scale_fill_manual("variant", values=c(head(colours_VARIANTS,-1),"black")) +
  scale_colour_manual("variant", values=c(head(colours_VARIANTS,-1),"black")) +
  theme(legend.position="right") 

ggsave(file="Re values per variant_avgRe_from_cases_with clipping.png", width=8, height=6)

week.readfile <- isoweek(Sys.Date())

setwd("../../../")

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Prevalence estimates - Week ", week.readfile))
push(repo, credentials = git.auth)
