temp = tail(list.files(path = "data-rivm/bco-settings/",pattern="*.csv", full.names = T),1)
myfiles = read.csv(temp)

bco.complete <- settings %>%
  filter(Source_and_contact_tracing_phase == "high")


bco.complete.schooldaycare <- bco.complete %>%
  filter(Setting_reported == "school_daycare")

safety.pop <- read.csv("misc/safetyregions-population.csv")

bco.school <- merge(bco.complete.schooldaycare,safety.pop, by = "Security_region_code")
bco.school$infections_100K <- bco.school$Number_settings_reported/bco.school$population*100000


# Plot for #patients in hospital per day
bco.plot <- bco.school %>%
  filter(Date_of_publication >= "2021-04-01") %>%
  ggplot(aes(x=Date_of_publication, y=infections_100K)) + 
  geom_line(aes(y = infections_100K, color = "Besmettingen op school per 100K inwoners"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_color_manual(values = c("#F58121")) +
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
  ggtitle("Besmettingen op school per 100.000 inwoners")

bco.plot + facet_wrap(~Security_region_name.x)  +
  ggsave("plots/bco_school.png")
