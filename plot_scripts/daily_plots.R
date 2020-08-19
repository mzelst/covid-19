require(cowplot)
require(tidyverse)
require(rjson)
require(data.table)
rm(list=ls())

# Script plots for daily update
all.data <- read.csv("data/all_data.csv")
all.data$date <- as.Date(all.data$date)
all.data <- all.data[order(all.data$date),]

filter.date <- Sys.Date()-28 # Set filter date for last 4 weeks


# Plot for positive tests per day
cases <- all.data %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=new.infection)) + 
  geom_line(aes(y = net.infection, color = "Toename besmettingen per dag (incl. correcties)"), lwd=1.2) +
  geom_line(aes(y = positive_7daverage, color = "Voortschrijdend gemiddelde (7 dagen)"), lwd=1.2) +
  geom_line(aes(y = new.infection, color = "Nieuw gemelde besmettingen per dag"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Besmettingen per dag",
       color = "Legend") +
  ggtitle("Meldingen van geconstateerde besmettingen") +
  ggsave("plots/positieve_tests_per_dag.png",width=12, height = 10)

# Plot for #patients in hospital per day
nice.today <- read.csv("data-nice/nice-today.csv")
nice.today$date <- as.Date(nice.today$date)

aanwezig <- nice.today %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=Hospital_Currently)) + 
  geom_line(aes(y = Hospital_Currently, color = "Aanwezig op verpleegafdeling"), lwd=1.2) +
  geom_line(aes(y = IC_Current, color = "Aanwezig op IC"), lwd=1.2) +
  ylim(0,350) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Totaal aanwezig",
       color = "Legend") +
  ggtitle("Aanwezig op de IC vs. verpleegafdeling") +
  ggsave("plots/overview_aanwezig_zkh.png", width = 15, height=4)

# Plot for #patients intake per day

opnames <- all.data %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=new.hospitals, group = 1)) + 
  geom_line(aes(y = new.hospitals, color = "Opname op verpleegafdeling"), lwd=1.2) +
  geom_line(aes(y = ic_intake_nice, color = "Opname op IC"), lwd=1.2) +
  ylim(0,15) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Opnames per dag",
       color = "Legend") +
  ggtitle("Opnames op de IC vs. verpleegafdeling") +
  ggsave("plots/overview_opnames_zkh.png", width = 15, height=4)

reproduction <- rjson::fromJSON(file = "https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

reproduction <- reproduction %>%
  mutate(Date = Date %>% as.Date)

prevalence <- rjson::fromJSON(file = "https://data.rivm.nl/covid-19/COVID-19_prevalentie.json",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

prevalence <- prevalence %>%
  mutate(groei_besmettelijken = c(0,diff(prev_avg)))

prevalence$besmet_7daverage <- frollmean(prevalence[,"groei_besmettelijken"],7)

reproduction <- reproduction %>%
  ggplot(aes(x=Date, y=Rt_avg, group = 1)) + 
  geom_line(aes(y = Rt_low), lwd=0.6) +
  geom_line(aes(y = Rt_up), lwd=0.6) +
  geom_ribbon(aes(ymin=Rt_low,ymax=Rt_up), fill="lightblue") +
  geom_line(aes(y = Rt_avg, color = "Effectieve R"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Reproductiegetal",
       color = "Legend") +
  ggtitle("Reproductiegetal") + 
  ggsave("plots/reproductie_getal.png",width=15, height = 4)  

# Merge plots into grid
plot.daily <- plot_grid( aanwezig + theme(legend.position="bottom"),
                         opnames + theme(legend.position="bottom"),
                         cases + theme(legend.position = "bottom", legend.direction = "vertical"),
                         reproduction + theme(legend.position="bottom"),
                         align = 'hv',
                         nrow = 2,
                         hjust = -1
)

# Save grid plot for daily use
save_plot("plots/plot_daily.png", plot.daily, base_asp = 1.1, base_height = 7, base_width = 10)

rm(list=ls())
