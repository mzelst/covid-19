library(tidyverse)
library(janitor)
require(gganimate)
rm(list=ls())

dat <- read.csv("data-rivm/casus-datasets/COVID-19_casus_landelijk_2020-08-10.csv") %>%
  dplyr::filter(Agegroup != "<50" & Agegroup != "Unknown")
dat$week <- strftime(dat$Date_statistics, format = "%V")
dat$value <- 1

dat_tidy <- aggregate(dat$value, by = list(Leeftijd = dat$Agegroup, Week = dat$week), FUN = sum)
dat_tidy$Week <- as.numeric(dat_tidy$Week)
colnames(dat_tidy) <- c("Leeftijd","Week","Besmettingen")


perc <- dat_tidy %>% 
  group_by(Week) %>% mutate(value = round((Besmettingen/sum(Besmettingen))*100,2))

dat_tidy <- cbind(dat_tidy[,c("Leeftijd","Week")],as.numeric(perc$value))
colnames(dat_tidy) <- c("Leeftijd","Week","value")

dat_wide <- dat_tidy %>%
  spread(Week,value = Besmettingen)


# The * 1 makes it possible to have non-integer ranks while sliding
dat_formatted <- dat_tidy %>%
  group_by(Week) %>%
  mutate(rank = rank(-value),
         Value_rel = value/value,
         Value_lbl = paste0(" ",round(value/1e9))) %>%
  group_by(Leeftijd) %>% 
  filter(rank <=12) %>%
  ungroup()


staticplot = ggplot(dat_formatted, aes(rank, group = Leeftijd, 
                                       fill = as.factor(Leeftijd), color = as.factor(Leeftijd))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Leeftijd, " ")), vjust = 0.2, hjust = 1, size = 12) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=40, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))


anim = staticplot + transition_states(Week, transition_length = 4, state_length = 4) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Week : {closest_state}',  
       subtitle  =  "Leeftijdsgroepen (%)",
       caption  = "Relatief aantal positieve tests per leeftijdsgroep")


animate(anim, 400, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("plots/age_bargraph_week32.gif"))

animate(anim, 400, fps = 20,  width = 1200, height = 1000, 
        renderer = av_renderer("age_corona_final.mp4"))
