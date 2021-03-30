require(EpiEstim)
require(scales)
require(psych)
require(incidence)

corona <- read.csv("corrections/cases_perday.csv")
covid.incidence <- corona[,c(1,3)]
#corona.breakdown <- read.csv("data-dashboards/date_statistics_mutations.csv")
#covid.incidence <- corona.breakdown[,c("Datum","DOO")]

colnames(covid.incidence) <- c("dates","I")
covid.incidence$dates <- as.Date(covid.incidence$dates)
covid.incidence$I <- as.numeric(covid.incidence$I)

covid.incidence <- covid.incidence %>%
  filter(dates > "2021-01-01")

#config <- make_config(list(mean_si = 4.0, std_mean_si = 0.3,
#                           min_mean_si = 3.5, max_mean_si = 4.5,
#                           std_si = 0.5, std_std_si = 0.15,
#                           min_std_si = 0.2, max_std_si = 0.5))


#res_parametric_si <- estimate_R(covid.incidence, 
                                #method="uncertain_si",
                                #config = config)

time <- 5

T <- nrow(covid.incidence)
t_start <- seq(2, T-time) # starting at 2 as conditional on the past observations
t_end <- t_start + time # adding 6 to get 7-day windows as bounds included in window

res_parametric_si <- wallinga_teunis(covid.incidence, method="parametric_si",
                       config = list(t_start = t_start, t_end = t_end,
                                     mean_si = 4.0, std_si = 0.3,
                                     n_sim = 10))


#plot(res_parametric_si, legend = FALSE)

covid.r <- res_parametric_si$R

dates.start <- 8-6+time

dates <- as.data.frame(covid.incidence[dates.start:(nrow(covid.incidence)),1])
colnames(dates) <- c("dates")

covid.r <- cbind(dates,covid.r)

r.rivm <- fromJSON(txt = "https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json")
r.rivm <- r.rivm[,c("Date","Rt_avg","Rt_low","Rt_up")]
colnames(r.rivm) <- c("dates","Rt_avg","Rt_low","Rt_up")
r.rivm$dates <- as.Date(r.rivm$dates)

covid.r <- merge(covid.r, r.rivm,by="dates")
covid.r <- covid.r[,c("dates","Mean(R)","Std(R)","Rt_avg","Rt_low","Rt_up")]
covid.r$R_lagged <- lead(covid.r$`Mean(R)`,2)
covid.r$R_Std_lag <- lead(covid.r$`Std(R)`,2)


covid.r$Rt_avg <- as.numeric(covid.r$Rt_avg)
covid.r$R_lagged <- as.numeric(covid.r$R_lagged)
covid.r$Std_R <- as.numeric(covid.r$`Std(R)`)
covid.r$R_est_up <- covid.r$R_lagged+covid.r$R_Std_lag*1.96
covid.r$R_est_low <- covid.r$R_lagged-covid.r$R_Std_lag*1.96

covid.r$diff <- round(covid.r$R_lagged-covid.r$Rt_avg,2)

R_last_estimate <- round(covid.r[nrow(covid.r)-11,"R_lagged"],2)

covid.r %>%
  filter(dates < Sys.Date()-14) %>%
  ggplot(aes(x=dates, y=Rt_avg)) + 
  geom_line(aes(y = Rt_avg, color = "R - RIVM"), lwd=0.8) +
  geom_line(aes(y = R_lagged, color = "R - Marino"), lwd=1.2) +
  #geom_line(aes(y = R_est_up, color = "R - CI (upper bound)"), lwd=1.2, linetype = "dashed") +
  #geom_line(aes(y = R_est_low, color = "R - CI (lower bound)"), lwd=1.2, linetype = "dashed") +
  scale_x_date(date_breaks = "1 week") +
  scale_y_continuous(limits = c(0.75, 1.35)) +
  scale_color_manual(values = c("#F58121","#228AC7")) +
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
        legend.direction = "horizontal",
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Besmettingen per dag",
       subtitle = paste0("Laatst berekende R = ",R_last_estimate, "\n Datum = ",as.Date(last(covid.r$dates)-11)),
       color = "Legend") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  ggtitle("Reproductiegetal") +
  ggsave("plots/reproductiegetal_marino.png")

covid.r$Std_R <- NULL
filename.repro.file <- paste0("data-misc/reproduction-numbers/marino/reproduction_number_",Sys.Date(),".csv")
write.csv(covid.r, file = filename.repro.file)