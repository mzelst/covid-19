require(emo)
require(ggtext)
require(emojifont)
library(ggplot2)
library(extrafont)
loadfonts(device = "win")


addSmallLegend <- function(myPlot, pointSize = 1, textSize = 18, spaceLegend = 1) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_blank(), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}

temp <- tempfile()
download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",temp)
data <- read.csv(unz(temp, "2020_NL_Region_Mobility_Report.csv"),sep=",")

google.mobility <- data[which(data$sub_region_1 == ""),]
google.mobility$date <- as.Date(google.mobility$date)

google.mobility <- google.mobility %>%
  mutate(retail_recreatie = round(frollmean(google.mobility[,"retail_and_recreation_percent_change_from_baseline"],7),1)) %>%
  mutate(supermarkt_apotheek = round(frollmean(google.mobility[,"grocery_and_pharmacy_percent_change_from_baseline"],7),1)) %>%
  mutate(parken = round(frollmean(google.mobility[,"parks_percent_change_from_baseline"],7),1)) %>%
  mutate(openbaar_vervoer = round(frollmean(google.mobility[,"transit_stations_percent_change_from_baseline"],7),1)) %>%
  mutate(werk = round(frollmean(google.mobility[,"workplaces_percent_change_from_baseline"],7),1)) %>%
  mutate(thuis = round(frollmean(google.mobility[,"residential_percent_change_from_baseline"],7),1))


google.plot <- google.mobility %>%
  filter(date > "2020-06-30") %>%
  ggplot(aes(x=date, y=retail_recreatie)) + 
  geom_line(aes(y = retail_recreatie, color = "Retail en Recreatie"), lwd=1.2) +
  geom_line(aes(y = supermarkt_apotheek, color = "Supermarkten en Apotheken"), lwd=1.2) +
  geom_line(aes(y = werk, color = "Werk"), lwd=1.2) +
  ggtitle("Mobiliteit - Afwijking t.o.v. 2019 (%)") + 
  theme_bw() + 
  scale_y_continuous(expand = c(0,10), limits = c(-40, NA)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=24),
        axis.text.y = element_text(size=24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 40),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.title.position = "plot",
        plot.caption = element_text(size = 16),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=20, color = "black"),
        legend.position = c(.12,.16),
        legend.margin = margin(3, 3, 3, 3)) + 
  labs(x = "Datum",
       y = "Mobiliteit",
       subtitle = "18-sep: kroeg uurtje eerder dicht \n28-sep: we gaan voor een R van 0.9 \n13-okt: gedeeltelijke lockdown",
       caption = "Bron: Google Mobility | Plot: @mzelst",
       color = "Legend") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = as.Date("2020-09-18"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2020-09-28"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2020-10-13"), linetype = "dotted") + 
  annotate("curve", x = as.Date("2020-10-15"), xend = as.Date("2020-10-17"), 
                       y = -32, yend = -18, curvature = 1.0,
           colour = "black", size=1.0, alpha=0.8, arrow =arrow(type = "open",length = unit(2,"mm")))
  

annotation <- data.frame(
  x = as.Date("2020-10-05"),
  y = c(-35),
  label = c("'Laatste biertje' effect")
)

google.plot <- google.plot + geom_text(data=annotation, aes( x=x, y=y, label=label), 
               color="black", 
               size=10,angle=0, family="serif",fontface="bold" ) 

addSmallLegend(google.plot) + 
  ggsave("plots/mobiliteit/google_mobility.png",
         width = 16, height = 10, units = "cm", device='png')


## Apple mobility

temp <- tempfile()
download.file("https://covid19-static.cdn-apple.com/covid19-mobility-data/2019HotfixDev16/v3/en-us/applemobilitytrends-2020-10-22.csv",temp)
apple.mobility <- read.csv(temp, sep=",")

apple.mobility <- apple.mobility[which(apple.mobility$region == "Netherlands"),]
apple.mobility <- apple.mobility[,7:(ncol(apple.mobility))]

apple.mobility <- as.data.frame(t(apple.mobility))
colnames(apple.mobility) <- c("driving","transit","walking")

apple.mobility$date <- row.names(apple.mobility)
apple.mobility$date <- gsub('X', '', apple.mobility$date)
apple.mobility$date <- gsub('[.]', '-', apple.mobility$date)
apple.mobility$date <- as.Date(apple.mobility$date)

apple.mobility <- apple.mobility %>%
  mutate(rijden = round(frollmean(apple.mobility[,"driving"],7),1)) %>%
  mutate(openbaar_vervoer = round(frollmean(apple.mobility[,"transit"],7),1)) %>%
  mutate(wandelen = round(frollmean(apple.mobility[,"walking"],7),1)) %>%
  mutate(rijden = rijden-100) %>%
  mutate(openbaar_vervoer = openbaar_vervoer-100) %>%
  mutate(wandelen = wandelen-100)

apple.plot <- apple.mobility %>%
  ggplot(aes(x=date, y=rijden)) + 
  geom_line(aes(y = rijden, color = "Rijden"), lwd=1.2) +
  geom_line(aes(y = openbaar_vervoer, color = "Openbaar vervoer"), lwd=1.2) +
  geom_line(aes(y = wandelen, color = "Wandelen"), lwd=1.2) +
  ggtitle("Mobiliteit - Afwijking t.o.v. 13 januari (%)") + 
  theme_bw() + 
  scale_y_continuous(expand = c(0,10), limits = c(-100, NA)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=24),
        axis.text.y = element_text(size=24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 40),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.title.position = "plot",
        plot.caption = element_text(size = 16),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=20, color = "black"),
        legend.position = c(.12,.20),
        legend.margin = margin(3, 3, 3, 3)) + 
  labs(x = "Datum",
       y = "Mobiliteit",
       subtitle = "18-sep: kroeg uurtje eerder dicht \n28-sep: we gaan voor een R van 0.9 \n13-okt: gedeeltelijke lockdown",
       caption = "Bron: Apple Mobility | Plot: @mzelst",
       color = "Legend") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = as.Date("2020-09-18"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2020-09-28"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2020-10-13"), linetype = "dotted")

addSmallLegend(apple.plot) + 
  ggsave("plots/mobiliteit/apple_mobility.png",
         width = 16, height = 10, units = "cm", device='png')


