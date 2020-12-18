require(stringr)
require(tidyr)
require(cbsodataR)
require(reshape2)
require(lubridate)

week.now <- isoweek(Sys.Date())
week.readfile <- isoweek(Sys.Date())-1

dat <- read.csv(paste0("workflow/excess_mortality/data/province_week_mortality_week",week.readfile,".csv"))
province.population <- read.csv('misc/provinces-population-year.csv')
province.population <- province.population %>%
  mutate(Province=recode(Province,
                         'Fryslân' = 'Friesland'))

province.population.2020 <- province.population %>%
  filter(Jaar == 2020) %>%
  mutate(Populatie2020 = aantal) %>%
  select(-c(Jaar,aantal))


mortality.province <- gather(dat, key = "Province", value = "deaths",Groningen:Limburg)
mortality.province <- mortality.province %>%
  mutate(Province=recode(Province,
                         'Noord.Brabant' = 'Noord-Brabant',
                         'Noord.Holland' = 'Noord-Holland',
                         'Zuid.Holland' = 'Zuid-Holland'))

mortality.province <- merge(mortality.province, province.population[,c("Province","Jaar","aantal")],by=c("Province","Jaar"))
mortality.province <- merge(mortality.province, province.population.2020, by = c("Province"))

mortality.province$deaths_std <- mortality.province$deaths/mortality.province$aantal*mortality.province$Populatie2020

mortality_wide <- dcast(mortality.province, Province + Week ~ Jaar, value.var = "deaths_std", sum)

mortality_wide$Average20172019 <- rowMeans(mortality_wide[,c("2017","2018","2019")])

mortality_wide$excess_death <- round(mortality_wide$`2020` - mortality_wide$Average20172019,0)
mortality_wide$excess_death_perc <- round(mortality_wide$`2020`/mortality_wide$Average20172019*100,3)-100

# Excess deaths - absolute
excess_deaths <- aggregate(excess_death ~ Province + Week, data = mortality_wide, FUN = sum)
excess_deaths_wide <- spread(excess_deaths, key = Province, value = excess_death)

excess_mort_stripped <- subset(excess_deaths_wide, Week < week.now)
excess_mort_long <- gather(excess_mort_stripped, key="Province", value="deaths", Drenthe:`Zuid-Holland`)

# Excess deaths - percentage

excess_deaths_perc <- aggregate(excess_death_perc ~ Province + Week, data = mortality_wide, FUN = sum)
excess_deaths_perc_wide <- spread(excess_deaths_perc, key = Province, value = excess_death_perc)

excess_mort_perc_stripped <- subset(excess_deaths_perc_wide, Week < week.now)
excess_mort_perc_long <- gather(excess_mort_perc_stripped, key="Province", value="deaths", Drenthe:`Zuid-Holland`)


# Plot excess deaths - absolute
excess_mort_plot <- excess_mort_long %>%
  ggplot(aes(x=Week, y=deaths)) + 
  geom_line(aes(y = deaths, color ="Oversterfte"), lwd=1.0) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Oversterfte",
       color = "Legend") +
  geom_hline(yintercept=0) +
  ggtitle("Oversterfte per provincie")

excess_mort_plot + facet_wrap(~Province)

# Plot excess deaths - percentage
excess_mort_perc_plot <- excess_mort_perc_long %>%
  ggplot(aes(x=Week, y=deaths)) + 
  geom_line(aes(y = deaths, color ="Oversterfte (%)"), lwd=1.0) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Oversterfte",
       color = "Legend") +
  geom_hline(yintercept=0) +
  ggtitle("Oversterfte per provincie (%)")

excess_mort_perc_plot + facet_wrap(~Province) + 
  ggsave("plots/excess_mortality_province.png",
         width = 16, height = 10, units = "cm", device='png')

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Excess mortality analyses - Week ", week.readfile))
push(repo, credentials = git.auth)
