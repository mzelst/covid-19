## Parse municipalities population data

dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c("2021MM02")))
dat.mun <- dat.mun[,c("RegioS","BevolkingAanHetEindeVanDePeriode_15")]
colnames(dat.mun) <- c("statcode","populatie")

dat.mun <- dat.mun %>%
  dplyr::filter(!is.na(populatie)) %>%
  dplyr::filter(grepl("GM", statcode))

gemeentegrenzen <- geojson_read("misc/maps/Gemeentegrenzen2021RD.geojson", what = "sp")
gemeentes <- gemeentegrenzen@data[,c(2,4)]

gemeente.stats <- merge(gemeentes, dat.mun, by = "statcode")
colnames(gemeente.stats) <- c("Municipality_code","Municipality_name","population")
write.csv(gemeente.stats, file = "misc/municipalities-population.csv")

## Parse GGD population data
nl_dt <- fread("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_2021-04-01.csv.gz")
nl_dt <- aggregate(Deceased ~ Municipal_health_service + Municipality_code, data = nl_dt, sum)

dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c("2021MM02")))
dat.mun <- dat.mun[,c("RegioS","BevolkingAanHetEindeVanDePeriode_15")]
colnames(dat.mun) <- c("statcode","populatie")

dat.mun <- dat.mun %>%
  dplyr::filter(!is.na(populatie)) %>%
  dplyr::filter(grepl("GM", statcode))

gemeentegrenzen <- geojson_read("misc/maps/Gemeentegrenzen2021RD.geojson", what = "sp")
gemeentes <- gemeentegrenzen@data[,c(2,4)]

gemeente.stats <- merge(gemeentes, dat.mun, by = "statcode")
colnames(gemeente.stats) <- c("Municipality_code","Municipality_name","population")

df <- merge(nl_dt, gemeente.stats, by = "Municipality_code")
df <- aggregate(population ~ Municipal_health_service, data = df, sum)

ggds_population <- read.csv("misc/ggds-population.csv")

df <- cbind(df, ggds_population[,c("population","ggd_code","ID")])
df <- df[,c(-3)]
colnames(df) <- c("statnaam","population","ggd_code","ID")
write.csv(df, file = "misc/ggds-population.csv", row.names = FALSE)

## Parse province data

dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c("2021MM02")))
dat.mun <- dat.mun[,c("RegioS","BevolkingAanHetEindeVanDePeriode_15")]
colnames(dat.mun) <- c("statcode","populatie")


