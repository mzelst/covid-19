# Script plots for daily update
all.data <- read.csv("data/all_data.csv")
all.data$date <- as.Date(all.data$date)
all.data <- all.data[order(all.data$date),]

testdata <- read.csv("data-dashboards/percentage-positive-daily-national.csv")
testdata$date <- as.Date(testdata$date)

testdata$values.infected_percentage <- testdata$values.infected_percentage/100
testdata$pos.rate.7d.avg <- testdata$pos.rate.7d.avg/100

filter.date <- Sys.Date()-56 # Set filter date for last 4 weeks

all.data[211,27] <- 12
all.data[212,27] <- 10

# Remove most recent NICE entry, since it's incomplete
last_date = last(all.data$date)
all.data[all.data$date == last_date, "Hospital_Intake"] <- NA
all.data[all.data$date == last_date, "IC_Intake"] <- NA
rm(last_date)

day.today <- wday(Sys.Date(), week_start = 2)

# Plot for positive tests per day
cases <- all.data %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=new.infection)) + 
  geom_line(aes(y = net.infection, color = "Toename besmettingen per dag (incl. correcties)"), lwd=1.2) +
  geom_line(aes(y = positive_7daverage, color = "Voortschrijdend gemiddelde (7 dagen)"), lwd=1.2) +
  geom_line(aes(y = new.infection, color = "Nieuw gemelde besmettingen per dag"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 200), limits = c(0, NA)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=12),
        axis.text.y = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Besmettingen per dag",
       subtitle = "Maandagen",
       color = "Legend") +
  geom_vline(xintercept = as.Date(Sys.Date()-56-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-49-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-42-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-35-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-28-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-21-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-14-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-7-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-day.today), linetype = "dotted") +
  ggtitle("Meldingen van geconstateerde besmettingen") +
  ggsave("plots/positieve_tests_per_dag.png",width=12, height = 8)

testplot.subtitle <- paste0("Let op: percentage is bekend t/m ",Sys.Date()-2," \n\n Maandagen")

# Plot for positive tests per day
testplot <- testdata %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=values.infected_percentage)) + 
  geom_line(aes(y = values.infected_percentage, color = "Percentage positief per dag (GGD)"), lwd=1.2) +
  geom_line(aes(y = pos.rate.7d.avg, color = "Percentage positief - Zwevend 7-daags gemiddelde (GGD)"), lwd=1.2) +
  scale_x_date(breaks = "1 weeks") + 
  scale_y_continuous(limits = c(0, 0.2), labels = scales::percent, breaks = seq(0,0.2,0.02)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=12),
        axis.text.y = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.pos = "bottom") +
  labs(x = "Datum",
       y = "Percentage positief per dag",
       subtitle = testplot.subtitle,
       color = "Legend") +
  geom_vline(xintercept = as.Date(Sys.Date()-56-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-49-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-42-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-35-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-28-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-21-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-14-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-7-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-day.today), linetype = "dotted") +
  ggtitle("Percentage positief per dag (GGD)") +
  ggsave("plots/percentage_positief_per_dag.png",width=12, height = 8)

# Plot for #patients in hospital per day
aanwezig <- all.data %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=Hospital_Currently)) + 
  geom_line(aes(y = Hospital_Currently, color = "Aanwezig op verpleegafdeling (NICE)"), lwd=1.2) +
  geom_line(aes(y = IC_Current, color = "Aanwezig op IC (NICE)"), lwd=1.2) +
  geom_line(aes(y = Kliniek_Bedden, color = "Aanwezig op verpleegafdeling (LCPS)"), lwd=1.2) +
  geom_line(aes(y = IC_Bedden_COVID, color = "Aanwezig op IC (LCPS)"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 50), limits = c(0, NA)) +
  scale_color_manual(values = c("#F58121", "#228AC7", "#f79a4d", "#7ab9dd")) +
  guides(colour = guide_legend(reverse=T)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Totaal aanwezig",
       color = "Legend") +
  ggtitle("Aanwezig op de verpleegafdeling vs. IC (NICE & LCPS)") +
  ggsave("plots/overview_aanwezig_zkh.png", width = 12, height=8)

# Plot for #patients intake per day
opnames <- all.data %>%
  filter(date > filter.date) %>%
  ggplot(aes(x=date, y=new.hospitals, group = 1)) + 
  geom_line(aes(y = hospital_intake_rivm, color = "Opname op verpleegafdeling (GGDs)"), lwd=1.2) +
  geom_line(aes(y = Kliniek_Nieuwe_Opnames_COVID, color = "Opname op verpleegafdeling (LCPS)"), lwd=1.2, position = position_nudge(x=-1)) +
  geom_line(aes(y = Hospital_Intake, color = "Opname op verpleegafdeling (NICE)"), lwd=1.2, na.rm = TRUE) +
  geom_line(aes(y = IC_Intake, color = "Opname op IC (NICE)"), lwd=1.2, na.rm = TRUE) +
  geom_line(aes(y = IC_Nieuwe_Opnames_COVID, color = "Opname op IC (LCPS)"), lwd=1.2, position = position_nudge(x=-1)) +
  scale_y_continuous(expand = c(0, 10), limits = c(0, NA)) +
  scale_color_manual(values = c("#F58121", "#228AC7", "#F9E11E", "#f79a4d", "#7ab9dd")) +
  guides(colour = guide_legend(reverse=T)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=12),
        axis.text.y = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Opnames per dag",
       subtitle = "Maandagen",
       color = "Legend") +
  geom_vline(xintercept = as.Date(Sys.Date()-56-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-49-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-42-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-35-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-28-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-21-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-14-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-7-day.today), linetype = "dotted") +
  geom_vline(xintercept = as.Date(Sys.Date()-day.today), linetype = "dotted") +
  ggtitle("Opnames op de verpleegafdeling en IC") +
  ggsave("plots/overview_opnames_zkh.png", width = 12, height=8)

reproduction <- rjson::fromJSON(file = "https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

reproduction <- reproduction %>%
  mutate(Date = Date %>% as.Date)

last.date.repro <- last(reproduction$Date)

# Write reproduction number data file for day
filename.daily.repro <- paste0("data-misc/reproduction-numbers/reproduction_number_",last.date.repro,".csv")
write.csv(reproduction, file = filename.daily.repro, row.names = F)

#prevalence <- rjson::fromJSON(file = "https://data.rivm.nl/covid-19/COVID-19_prevalentie.json",simplify=TRUE) %>%
#  map(as.data.table) %>%
#  rbindlist(fill = TRUE)

#prevalence <- prevalence %>%
#  mutate(groei_besmettelijken = c(0,diff(prev_avg))) %>%
#  mutate(Date = Date %>% as.Date)

#prevalence$besmet_7daverage <- frollmean(prevalence[,"groei_besmettelijken"],7)

#reproduction <- reproduction %>%
#  ggplot(aes(x=Date, y=Rt_avg, group = 1)) + 
#  geom_line(aes(y = Rt_low), lwd=0.6) +
#  geom_line(aes(y = Rt_up), lwd=0.6) +
#  geom_ribbon(aes(ymin=Rt_low,ymax=Rt_up), fill="lightblue") +
#  geom_line(aes(y = Rt_avg, color = "Effectieve R"), lwd=1.2) +
#  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#  theme(axis.title.x=element_blank(),
#        axis.title.y=element_blank(),
#        legend.pos = "bottom",
#        legend.direction = "vertical",
#        legend.title = element_blank()) +
#  labs(x = "Datum",
#       y = "Reproductiegetal",
#       color = "Legend") +
#  ggtitle("Reproductiegetal") + 
#  ggsave("plots/reproductie_getal.png",width=15, height = 4)

#filter.date <- Sys.Date()-56 # Set filter date for last 8 weeks

#prevalence %>%
#  ggplot(aes(x=Date, y=prev_avg, group = 1)) + 
#  geom_line(aes(y = prev_low), lwd=0.6) +
#  geom_line(aes(y = prev_up), lwd=0.6) +
#  geom_ribbon(aes(ymin=prev_low,ymax=prev_up), fill="lightblue") +
#  geom_line(aes(y = prev_avg, color = "Aantal besmettelijke mensen"), color = "blue",lwd=1) +
#  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#  theme(axis.title.x=element_blank(),
#        axis.title.y=element_blank(),
#        legend.pos = "bottom",
#        legend.direction = "vertical",
#        legend.title = element_blank(),
#        panel.background = element_rect(fill = "white",
#                                        colour = "white",
#                                        size = 0.5, linetype = "solid"),
#        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
#                                        colour = "grey"), 
#        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
#                                        colour = "grey")) +
#  labs(x = "Datum",
#       y = "Aantal besmettelijke mensen",
#       color = "Legend") +
#  ggtitle("Aantal besmettelijke mensen in Nederland") + 
#  ggsave("plots/prevalentie_overzicht.png",width=15, height = 4)

# Merge plots into grid
#plot.daily <- plot_grid( aanwezig + theme(legend.position="bottom"),
#                         opnames + theme(legend.position="bottom"),
#                         cases + theme(legend.position = "bottom", legend.direction = "vertical"),
#                         align = 'hv',
#                         nrow = 2,
#                         hjust = -1
#)

# Save grid plot for daily use
#save_plot("plots/plot_daily.png", plot.daily, base_asp = 1.1, base_height = 7, base_width = 10)

rm(aanwezig, all.data, cases, opnames, testdata, testplot, filter.date, testplot.subtitle)
