## Parse municipalities population data

dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c("2021MM04")))
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
nl_dt <- fread("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_2021-06-01.csv.gz")
nl_dt <- aggregate(Deceased ~ Municipal_health_service + Municipality_code, data = nl_dt, sum)

dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c("2021MM04")))
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
dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c("2021MM04")), RegioS = has_substring(c("PV")))
dat.mun <- dat.mun[,c("RegioS","BevolkingAanHetBeginVanDePeriode_1")]
dat.mun$RegioS <- as.character(dat.mun$RegioS)
colnames(dat.mun) <- c("RegioS","population")
dat.mun$RegioS <- gsub(" ", "", dat.mun$RegioS, fixed = TRUE)

province.identifier <- cbs_get_data("70072ned",add_column_labels = FALSE,Perioden = has_substring(c("2021JJ00")))
province.identifier <- province.identifier[,c("Code_289","Naam_290")]
province.identifier <- province.identifier %>% distinct()
colnames(province.identifier) <- c("RegioS","Province")
province.identifier$RegioS <- gsub(" ", "", province.identifier$RegioS, fixed = TRUE)
province.identifier$Province <- gsub(" ", "", province.identifier$Province, fixed = TRUE)

str(dat.mun)
str(province.identifier)
df <- merge(province.identifier, dat.mun, by = c("RegioS"), all.y=T)

df <- df[order(df$Province),]
df$ID <- seq(1,12)

write.csv(df, file = "misc/provinces-population.csv")

## Parse safety region data

dat.safetyregion <- cbs_get_data("84929NED",add_column_labels = FALSE)[,c("Code_1","Naam_2","Code_14","Naam_15","Code_26","Naam_27","Code_44","Naam_45","Inwonertal_52")]
pop.safetyregion <- aggregate(Inwonertal_52 ~ Code_44 + Naam_45, data = dat.safetyregion, FUN = sum)
colnames(pop.safetyregion) <- c("Security_region_code","Security_region_name","population")

pop.safetyregion$Security_region_name <- trimws(pop.safetyregion$Security_region_name)
pop.safetyregion$Security_region_code <- trimws(pop.safetyregion$Security_region_code)

write.csv(pop.safetyregion, file = "misc/safetyregions-population.csv")


## Age population data
pop.age <- cbs_get_data("83482NED",add_column_labels = FALSE,Perioden = has_substring(c("2021MM05")), 
                        Migratieachtergrond = has_substring(c("T001040")),
                        Generatie = has_substring(c("T001040")),
                        Geslacht = has_substring(c("T001038")))




## Repo

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

##Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Monthly update population data"))
push(repo, credentials = git.auth)

