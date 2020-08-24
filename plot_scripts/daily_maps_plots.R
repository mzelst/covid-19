library(jsonlite)
library(geojsonio)
library(sp)
rm(list=ls())

# De geodata wordt via de API van het Nationaal Georegister van PDOK opgehaald.
# Een overzicht van beschikbare data staat op https://www.pdok.nl/datasets.
geoUrl <- "https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2020_gegeneraliseerd&outputFormat=json"
gemeentegrenzen <- geojson_read("misc/maps/gemeentegrenzen2020.geojson", what = "sp")
dat.wide <- read.csv("data/municipality-today-detailed.csv")

cbs.data.plot <- dat.wide[,c("Municipality_code","rel_increase_1d","rel_increase_7d")]
colnames(cbs.data.plot) <- c("statcode","Besmettingen (sinds gisteren)","Besmettingen (7 dagen)")
cbs.data.plot$`Besmettingen (sinds gisteren)` <- ifelse((cbs.data.plot$`Besmettingen (sinds gisteren)`) < 0, 0, cbs.data.plot$`Besmettingen (sinds gisteren)`)
cbs.data.plot$`Besmettingen (7 dagen)` <- ifelse((cbs.data.plot$`Besmettingen (7 dagen)`) < 0, 0, cbs.data.plot$`Besmettingen (7 dagen)`)

gemeentegrenzen@data <- gemeentegrenzen@data %>%
  left_join(cbs.data.plot,by=c("statcode"))

g <- fortify(gemeentegrenzen, region = "id")
gemeentegrenzenDF <- merge(g, gemeentegrenzen@data, by = "id")

mun.map.today <- ggplot(data = gemeentegrenzenDF) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = `Besmettingen (sinds gisteren)`), color = "black", lwd=0.2) +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        legend.title = element_blank()) +
  ggtitle("Nieuwe besmettingen per gemeente per 100.000 inwoners") +
  theme_void() +
  scale_fill_gradientn(colours=c("white","yellow","orange", "red")) +
  ggsave("plots/gemeente_per_dag.png",width=5, height = 8)

mun.map.week <- ggplot(data = gemeentegrenzenDF) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = `Besmettingen (7 dagen)`), color = "black", lwd=0.2) +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        legend.title = element_blank()) +
  ggtitle("Nieuwe besmettingen per gemeente per 100.000 inwoners") +
  theme_void() +
  scale_fill_gradientn(colours=c("white","yellow","orange", "red")) +
  ggsave("plots/gemeente_per_week.png",width=5, height = 8)


## GGDS
geoUrl <- "https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_ggdregio_2019_gegeneraliseerd&outputFormat=json"
fileName <- "misc/maps/ggdgrenzen2020.geojson" # File for GGD map
ggdgrenzen <- geojson_read(fileName, what = "sp") # Load GGD map

temp = list.files(path = "data-rivm/casus-datasets/",pattern="*.csv", full.names = T) ## Pull names of all available datafiles 
dat.today <- read.csv(last(temp)) ## Case file today
dat.yesterday <- read.csv(temp[length(temp)-1]) ## Case file week ago
dat.weekago <- read.csv(temp[length(temp)-7]) ## Case file week ago

ggd.cases.today <- as.data.frame(table(dat.today$Municipal_health_service)) ## Cumulative cases per GGD
ggd.cases.yesterday <- as.data.frame(table(dat.yesterday$Municipal_health_service)) ## Cumulative cases per GGD
ggd.cases.weekago <- as.data.frame(table(dat.weekago$Municipal_health_service)) ## Cumulative cases per GGD - week ago

ggd_datalist <- list(as.data.frame(table(dat.today$Municipal_health_service)), as.data.frame(table(dat.yesterday$Municipal_health_service)),as.data.frame(table(dat.weekago$Municipal_health_service)))

ggd.data <- Reduce(
  function(x, y, ...) merge(x, y, by="Var1", ...),
  ggd_datalist
)

colnames(ggd.data) <- c("statnaam","cases_today","cases_yesterday","cases_lastweek")
ggd.data$statnaam <- as.character(ggd.data$statnaam)

ggd.data$statnaam <- recode(ggd.data$statnaam, "GGD FryslÃ¢n" = "GGD Fryslân",
                                 "GGD Zuid Limburg" = "GGD Zuid-Limburg",
                                 "GGD Brabant Zuid-Oost " = "GGD Brabant-Zuidoost",
                                 "GGD Hollands Midden" = "GGD Hollands-Midden",
                                 "GGD Limburg Noord" = "GGD Limburg-Noord",
                                 "GGD Zaanstreek-Waterland" = "GGD Zaanstreek/Waterland",
                                 "GGD West Brabant" = "GGD West-Brabant",
                                 "GGD Rotterdam Rijnmond" = "GGD Rotterdam-Rijnmond",
                                 "GGD regio Utrecht" = "GGD Regio Utrecht",
                                 "GGD Gelderland-Midden" = "Veiligheids- en Gezondheidsregio Gelderland-Midden",
                                 "GGD Hollands Noorden" = "GGD Hollands-Noorden",
                                 "GGD Noord en Oost Gelderland" = "GGD Noord- en Oost-Gelderland")


ggd.data <- ggd.data[order(ggd.data$statnaam),]
ggd.data$ID <- seq.int(nrow(ggd.data))

ggd.population <- read.csv("misc/ggds-population.csv")
ggd.data <- merge(ggd.data,ggd.population[,c("ID","population")],by = "ID", all=TRUE)

ggd.data$toename.vandaag <- ggd.data$cases_today - ggd.data$cases_yesterday
ggd.data$toename.vandaag <- ifelse((ggd.data$cases_today - ggd.data$cases_yesterday)<0,0,ggd.data$cases_today - ggd.data$cases_yesterday)

ggd.data$toename.week <- ggd.data$cases_today - ggd.data$cases_lastweek

ggd.data$`Besmettingen (sinds gisteren)` <- ggd.data$toename.vandaag/ggd.data$population*100000
ggd.data$`Besmettingen (7 dagen)` <- ggd.data$toename.week/ggd.data$population*100000


ggdgrenzen@data <- ggdgrenzen@data %>%
  left_join(ggd.data,by=c("statnaam"))

g.ggd <- fortify(ggdgrenzen, region = "id")
ggdDF <- merge(g.ggd, ggdgrenzen@data, by = "id")


ggd.map.today <- ggplot(data = ggdDF) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = `Besmettingen (sinds gisteren)`), color = "black", lwd=0.2) +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        legend.title = element_blank()) +
  ggtitle("Nieuwe besmettingen per GGD regio per 100.000") +
  theme_void() +
  scale_fill_gradientn(colours=c("white","yellow","orange", "red")) +
  ggsave("plots/ggd_per_dag.png",width=5, height = 8)

ggd.map.week <- ggplot(data = ggdDF) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = `Besmettingen (7 dagen)`), color = "black", lwd=0.2) +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        legend.title = element_blank()) +
  ggtitle("Nieuwe besmettingen per GGD regio per 100.000") +
  theme_void() +
  scale_fill_gradientn(colours=c("white","yellow","orange", "red")) +
  ggsave("plots/ggd_per_week.png",width=5, height = 8)

# Merge plots into grid
plot.daily.maps <- plot_grid(mun.map.today + theme(legend.position="right"),
                         mun.map.week + theme(legend.position="right"),
                         align = 'hv',
                         nrow = 1,
                         hjust = -1
)

# Save grid plot for daily use
save_plot("plots/plot_daily_maps.png", plot.daily.maps, base_asp = 1.1, base_height = 7, base_width = 10)

rm(list=ls())

