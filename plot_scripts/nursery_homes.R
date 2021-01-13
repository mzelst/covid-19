require(rgeos)
require(geojsonio)

#geoUrl <- "https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_veiligheidsregio_2020_gegeneraliseerd&outputFormat=json"
#download.file(geoUrl,destfile = "misc/maps/vrgrenzen2020.geojson")

nursery.locations <- read.csv("misc/nursery-homes-locations.csv")

fileName <- "misc/maps/vrgrenzen2020.geojson" # File for GGD map
vrgrenzen <- geojson_read(fileName, what = "sp") # Load veiligheidsregio map

#fileName <- "misc/maps/vrlabelpoint2020.geojson" # File for GGD map
#vrlabelpoint <- geojson_read(fileName, what = "sp") # Load veiligheidsregio map


temp = tail(list.files(path = "data-rivm/nursing-homes-datasets/",pattern="*.csv.gz", full.names = T),1)
dat.today <- read.csv(temp)
date.today <- as.Date(Sys.Date())

test.df <- dat.today %>%
  filter(Date_of_statistic_reported == date.today) %>%
  select(Security_region_code, Security_region_name,Total_infected_locations_reported) %>%
  mutate(statcode = Security_region_code)
test.df <- test.df[-26,]
test.df <- merge(test.df,nursery.locations[,c("Security_region_code","Aantal_locaties",by="Security_region_code")])

national.perc <- paste0("\nTotaal aantal besmette locaties: ",sum(test.df$Total_infected_locations_reported)," (",
                        round(sum(test.df$Total_infected_locations_reported/2452*100),0),"%)")

test.df$Percentage <- round(test.df$Total_infected_locations_reported/test.df$Aantal_locaties*100,0)

vr.codes <- as.data.frame(test.df$Security_region_code)
colnames(vr.codes) <- "Security_region_code"

vrgrenzen@data <- vrgrenzen@data %>%
  left_join(test.df,by=c("statcode"))

g.vr <- fortify(vrgrenzen, region = "id")
vrDF <- merge(g.vr, vrgrenzen@data, by = "id")


centroids.df <- as.data.frame(coordinates(vrgrenzen))
names(centroids.df) <- c("long", "lat") 
popList <- vrgrenzen@data$Percentage

idList <- vrgrenzen@data$statcode

pop.df <- data.frame(id = idList, population = popList, centroids.df)


nursery.map <- ggplot(data = vrDF) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = Percentage), lwd=0.2) +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        legend.title = element_blank()) +
  ggtitle("Percentage besmette verpleeghuislocaties per veiligheidsregio") +
  theme_void() +
  scale_fill_gradient2(low="green",mid="yellow",high="red",midpoint=5)
  
#scale_fill_gradientn(colours=c("lightgreen","yellow","orange", "red"))

nursery.map + geom_text(data=pop.df, aes(label=paste0(population,"%"), x=long, y=lat), size = 3.5, colour="black",fontface="bold") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, size = 14)) +
  labs(subtitle = national.perc) +
  ggsave("plots/nursery_homes_vr_map.png",width = 8, height=12)

