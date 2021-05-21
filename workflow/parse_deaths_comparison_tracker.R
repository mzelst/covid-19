## Deaths NICE ##
temp = tail(list.files(path = "data-nice/exit/Clinical_Beds",pattern="*.csv", full.names = T),1)
deaths_clinic <- fread(temp)[,c("date","Overleden")]
temp = tail(list.files(path = "data-nice/exit/IC",pattern="*.csv", full.names = T),1)
deaths_IC <- fread(temp)[,c("date","Overleden")]

deaths_nice <- merge(deaths_clinic, deaths_IC, by = "date")
deaths_nice$deaths_nice <- round((deaths_nice$Overleden.x+deaths_nice$Overleden.y)*0.95,0)
deaths_nice <- deaths_nice %>%
  mutate(deaths_nice = c(0,diff(deaths_nice))) %>%
  mutate(Week = isoweek(date)) %>%
  mutate(Year = isoyear(date))

deaths_nice <- aggregate(deaths_nice ~ Week + Year, data = deaths_nice, FUN = sum)

## Deaths nursing homes
temp = tail(list.files(path = "data-rivm/nursing-homes-datasets/",pattern="*.csv.gz", full.names = T),1)
nursing.homes <- fread(temp)

nursing.homes$Date_of_statistic_reported <- as.Date(nursing.homes$Date_of_statistic_reported)
nursing.homes.deaths.wide <- aggregate(Total_deceased_reported ~ Date_of_statistic_reported, data = nursing.homes, FUN = sum)
nursing.homes.deaths.wide <- nursing.homes.deaths.wide %>%
  mutate(Week = isoweek(Date_of_statistic_reported)) %>%
  mutate(Year = isoyear(Date_of_statistic_reported))

week_deaths_nursery <- aggregate(Total_deceased_reported ~ Week + Year, data = nursing.homes.deaths.wide, FUN = sum)
colnames(week_deaths_nursery) <- c("Week","Year","deaths_nursing")
deaths_total <- merge(deaths_nice,week_deaths_nursery, by = c("Week","Year"))

## 70 plus living at home

## Deaths nursing homes
temp = tail(list.files(path = "data-rivm/70plus-living-at-home-per-day/",pattern="*.csv.gz", full.names = T),1)
living.home_70 <- fread(temp)

living.home_70$Date_of_statistic_reported <- as.Date(living.home_70$Date_of_statistic_reported)
living.home_70.wide <- aggregate(Total_deceased_reported ~ Date_of_statistic_reported, data = living.home_70, FUN = sum)
living.home_70.wide <- living.home_70.wide %>%
  mutate(Week = isoweek(Date_of_statistic_reported)) %>%
  mutate(Year = isoyear(Date_of_statistic_reported))

week_deaths_living_home_70 <- aggregate(Total_deceased_reported ~ Week + Year, data = living.home_70.wide, FUN = sum)
colnames(week_deaths_living_home_70) <- c("Week","Year","deaths_living_home_70")

deaths_total <- merge(deaths_total,week_deaths_living_home_70, by = c("Week","Year"))

## RIVM all deaths

df_deaths_rivm <- read.csv("corrections/deaths_perweek.csv")[,c("Week","Year","weekdeath_today")]
deaths_total <- merge(deaths_total, df_deaths_rivm, by = c("Week","Year"), all.x=T)
colnames(deaths_total) <- c("Week","Year","deaths_nice","deaths_nursing","deaths_living_home_70","deaths_rivm")
setorder(deaths_total, Year,Week)

deaths_total$deaths_nonnursing_RIVM <- deaths_total$deaths_rivm-deaths_total$deaths_nursing
deaths_total$deaths_nice_nursing <- deaths_total$deaths_nice + deaths_total$deaths_nursing

## Deaths excess DLM / CBS

excess_dlm <- read.csv("data-misc/excess_mortality/excess_mortality_dlm_2021.csv")[,c("week","year","deaths_week_mid")]
excess_dlm$deaths_week_mid <- round(excess_dlm$deaths_week_mid)
colnames(excess_dlm) <- c("Week","Year","total_covid_mortality")
deaths_total <- merge(deaths_total,excess_dlm,by=c("Week","Year"), all.x=T)

## Deaths WLZ vs. other / CBS

u.cbs <- "https://www.cbs.nl/nl-nl/nieuws/2021/18/bijna-4-4-duizend-mensen-overleden-aan-covid-19-in-januari"
webpage.cbs <- read_html(u.cbs)

cbs.death.statistics <- as.data.frame(html_table(webpage.cbs)[[2]])
colnames(cbs.death.statistics) <- c("Year","Week","wlz_deaths_perc","other_deaths_perc")
cbs.death.statistics <- mutate_all(cbs.death.statistics, function(x) as.numeric(as.character(x)))
cbs.death.statistics <- cbs.death.statistics %>%
  mutate(wlz_deaths_perc = wlz_deaths_perc/100) %>%
  mutate(other_deaths_perc = other_deaths_perc/100)

u.cbs.week <- "https://www.cbs.nl/nl-nl/nieuws/2021/18/sterfte-in-week-17-hoger-dan-verwacht"
webpage.cbs.week <- read_html(u.cbs.week)

cbs.death.statistics.week <- as.data.frame(html_table(webpage.cbs.week)[[2]])[,c(1:3,6)]
colnames(cbs.death.statistics.week) <- c("Year","Week","wlz_deaths","other_deaths")
cbs.death.statistics.week <- mutate_all(cbs.death.statistics.week, function(x) as.numeric(as.character(x)))
cbs.death.statistics.week <- cbs.death.statistics.week[complete.cases(cbs.death.statistics.week),]


cbs.df <- merge(cbs.death.statistics,cbs.death.statistics.week, by = c("Week","Year"))
cbs.df <- cbs.df %>%
  mutate(wlz_covid = round(wlz_deaths*wlz_deaths_perc,0)) %>%
  mutate(other_covid = round(other_deaths*other_deaths_perc,0)) %>%
  select(Year, Week, wlz_covid, other_covid) %>%
  arrange(Year, Week)

rm(cbs.death.statistics, cbs.death.statistics.week, webpage.cbs, webpage.cbs.week, u.cbs, u.cbs.week)

deaths_total <- merge(deaths_total,cbs.df,by=c("Week","Year"), all.x=T)

setorder(deaths_total, Year, Week)

deaths_total$excess_rivm_nice <- deaths_total$deaths_nice-deaths_total$deaths_nonnursing_RIVM
deaths_total$week_year <- ifelse(deaths_total$Week<10,
                                 paste0(deaths_total$Year,"-",0,deaths_total$Week),
                                 paste0(deaths_total$Year,"-",deaths_total$Week))

deaths_total <- deaths_total %>% 
  mutate(deaths_home = total_covid_mortality - deaths_nice - wlz_covid) %>%
  mutate(deaths_home_perc = round(deaths_home/total_covid_mortality*100,3)) %>%
  mutate(deaths_nice_perc = round(deaths_nice/total_covid_mortality*100,3)) %>%
  mutate(deaths_wlz_perc = round(wlz_covid/total_covid_mortality*100,3)) %>%
  mutate(deaths_estimate = round(deaths_nonnursing_RIVM*1.8+deaths_nursing*1.8,0)) %>%
  mutate(deaths_estimate_2 = round((deaths_nice + deaths_nursing*1.8)*1.1,0)) %>%
  mutate(deaths_estimate_3 = round(deaths_nice + (deaths_nice/3) + deaths_nursing*1.8,0)) %>%
  mutate(cbs_rivm_factor = round(total_covid_mortality/deaths_rivm,2)) %>%
  mutate(factor_wlz = round(wlz_covid/deaths_nursing,2)) %>%
  mutate(factor_other = round(other_covid/deaths_nonnursing_RIVM,2)) %>%
  mutate(deviation_estim = round((deaths_estimate_3-total_covid_mortality)/total_covid_mortality*100,0))

write.csv(deaths_total, file = "corrections/death_week_comparisons.csv", row.names = F)

rm(deaths_clinic, deaths_IC,deaths_nice,df_deaths_rivm,excess_dlm,nursing.homes,nursing.homes.deaths.wide,
   week_deaths_nursery, living.home_70, living.home_70.wide,week_deaths_living_home_70,temp, cbs.df)


## PLOTS

cols <- c("#009E73", "#87109A","#E6830C","#D96DEA", "#2231C5","#000000")


plot <- deaths_total %>%
  filter(week_year <= "2021-04") %>%
  ggplot(aes(x=factor(week_year), y=deaths_rivm, group = 1)) + 
  geom_line(aes(y = deaths_rivm, color = "RIVM"), lwd=1.2) +
  geom_line(aes(y = total_covid_mortality, color = "CBS"), lwd=1.2) +
  theme_classic()+
  xlab("")+
  ylab("")+
  labs(title = "Sterfte per week",
       subtitle = "CBS data beschikbaar t/m januari 2021",
       caption = paste("Bron: CBS/RIVM | Plot: @mzelst  | ",Sys.Date())) +
  theme(
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "horizontal",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"),
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title =     element_text(hjust = 0.5 ,size = 24 ,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5   ,size = 12 ,color = "black", face = "italic"),
    axis.text.x = element_text(size=10,color = "black",face = "bold", angle = 90),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

plot + scale_colour_manual(values = cols) +
  ggsave("plots/sterfte_per_week_30K.png", width = 12, height=8)


## Percentages

plot <- deaths_total %>%
  filter(week_year >= "2020-39") %>%
  filter(week_year <= "2021-04") %>%
  ggplot(aes(x=factor(week_year), y=deaths_wlz_perc, group = 1)) + 
  geom_line(aes(y = deaths_wlz_perc, color = "Verpleeghuis"), lwd=1.2) +
  geom_line(aes(y = deaths_home_perc, color = "Thuis"), lwd=1.2) +
  geom_line(aes(y = deaths_nice_perc, color = "Ziekenhuis"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks = 10) +
  theme_classic()+
  xlab("")+
  ylab("")+
  labs(title = "Sterfte per plaats van overlijden",
       subtitle = "Percentage van totale sterfte",
       caption = paste("Bron: CBS/RIVM/NICE | Plot: @mzelst  | ",Sys.Date())) +
  theme(
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "horizontal",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"),
    legend.text = element_text(size = 12),
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title =     element_text(hjust = 0.5 ,size = 24 ,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5   ,size = 12 ,color = "black", face = "italic"),
    axis.text.x = element_text(size=10,color = "black",face = "bold", angle = 90),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

plot + scale_colour_manual(values = cols) +
  ggsave("plots/sterfte_per_week_30K_percentage.png", width = 12, height=8)



## Percentages

deaths_total <- deaths_total[-c(nrow(deaths_total)),]

plot <- deaths_total %>%
  filter(week_year >= "2020-39") %>%
  ggplot(aes(x=factor(week_year), y=deaths_nursing, group = 1)) + 
  geom_line(aes(y = deaths_nursing, color = "Verpleeghuis"), lwd=1.2) +
  geom_line(aes(y = deaths_nice, color = "Ziekenhuis"), lwd=1.2) +
  geom_line(aes(y = deaths_rivm, color = "Totaal (RIVM)"), lwd=1.2) +
  geom_line(aes(y = deaths_estimate_3, color = "Totaal (Schatting)"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1200), n.breaks = 10) +
  theme_classic()+
  xlab("")+
  ylab("")+
  labs(title = "Sterfte per groep",
       subtitle = "Sterfte per week",
       caption = paste("Bron: CBS/RIVM/NICE | Plot: @mzelst  | ",Sys.Date())) +
  theme(
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "horizontal",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"),
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title =     element_text(hjust = 0.5 ,size = 24 ,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5   ,size = 12 ,color = "black", face = "italic"),
    axis.text.x = element_text(size=10,color = "black",face = "bold", angle = 90),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

plot + scale_colour_manual(values = cols) +
  ggsave("plots/sterfte_per_week_30K_totalen.png", width = 12, height=8)


cbs.filter <- deaths_total %>%
  filter(Year == 2021 & Week >= 5)
cbs.filter$cumulative_deaths <- cumsum(cbs.filter$deaths_estimate_3) + 24484
deaths_total <- merge(deaths_total, cbs.filter[,c("Week","Year","cumulative_deaths")], by = c("Week","Year"),all.x=T)
setorder(deaths_total, Year, Week)

rm(deaths_total, plot, cols, dat)

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Update death comparison tracker for Twitter thread"))
push(repo, credentials = git.auth)

  