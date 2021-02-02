temp = last(list.files(path = "data-rivm/casus-datasets/",pattern="*.csv.gz", full.names = T),2) ## Pull names of all available datafiles
myfiles = lapply(temp, fread)

df <- map_dfr(myfiles, ~{
  .x
})
df$value <- 1
df$date <- as.Date(parse_date_time(df$Date_file, "Ymd HMS"))

df_date_long <- dcast.data.table(df, Date_statistics_type + Date_statistics + date ~ value, fun.aggregate = sum)
colnames(df_date_long) <- c("Type_Datum","Datum","Dag","x")
setorder(df_date_long, Dag ,Datum)


df_date_wide <- spread(df_date_long, key = Dag, value = x)
setDF(df_date_wide)

df_date_wide$Verschil <- df_date_wide[,ncol(df_date_wide)] - df_date_wide[,ncol(df_date_wide)-1]
df_date_wide <- df_date_wide[,c("Datum","Verschil","Type_Datum")]

df.final <- spread(df_date_wide, key = Type_Datum, value = Verschil, fill = 0)
colnames(df.final) <- c("Datum","DON_diff","DOO_diff","DPL_diff")
df.final$Datum <- as.Date(df.final$Datum)

temp = last(list.files(path = "data-rivm/casus-datasets/",pattern="*.csv", full.names = T)) ## Pull names of all available datafiles
dat.today <- setDT(as.data.frame(myfiles[2]))
dat.today$value <- 1

date_type.df <- dcast.data.table(dat.today, Date_statistics + Date_statistics_type ~ value, fun.aggregate = sum)
date_type_wide <- spread(date_type.df, key = Date_statistics_type, value = `1`, fill = 0)
date_type_wide$Datum <- as.Date(date_type_wide$Date_statistics)
date_type_wide <- date_type_wide[,c("DON","DOO","DPL","Datum")]

dat_wide <- merge(date_type_wide, df.final, by = "Datum")

write.csv(dat_wide, file = "data-dashboards/date_statistics_mutations.csv")
repo <- init()
add(repo, path = "data-dashboards/date_statistics_mutations.csv")
commit(repo, all = T, paste0("Update mutations per date_statistics types ",Sys.Date()))
push(repo, credentials = git.auth)
