temp = last(list.files(path = "data-rivm/casus-datasets/",pattern="*.csv.gz", full.names = T),2) ## Pull names of all available datafiles
myfiles = lapply(temp, fread)


df <- map_dfr(myfiles, ~{
  .x
})
df$value <- 1
df <- df[, date := as.Date(Date_file[1], format = "%Y-%m-%d"), by = Date_file]

df_date_long <- aggregate(df$value, by = list(Type_Datum = df$Date_statistics_type, Datum = df$Date_statistics, Dag = df$date), FUN = sum)

df_date_wide <- spread(df_date_long, key = Dag, value = x)

df_date_wide$Verschil <- df_date_wide[,ncol(df_date_wide)] - df_date_wide[,ncol(df_date_wide)-1]
df_date_wide <- df_date_wide[,c("Datum","Verschil","Type_Datum")]

df.final <- spread(df_date_wide, key = Type_Datum, value = Verschil, fill = 0)
colnames(df.final) <- c("Datum","DON_diff","DOO_diff","DPL_diff")
df.final$Datum <- as.Date(df.final$Datum)

temp = last(list.files(path = "data-rivm/casus-datasets/",pattern="*.csv", full.names = T)) ## Pull names of all available datafiles
dat.today <- fread(temp)

date_type.df <- as.data.frame(table(dat.today$Date_statistics, dat.today$Date_statistics_type))
date_type_wide <- spread(date_type.df, key = Var2, value = Freq)
date_type_wide$Datum <- as.Date(date_type_wide$Var1)
date_type_wide <- date_type_wide[,c("DON","DOO","DPL","Datum")]

dat_wide <- merge(date_type_wide, df.final, by = "Datum")

write.csv(dat_wide, file = "data-dashboards/date_statistics_mutations.csv")
add(repo, path = "data-dashboards/date_statistics_mutations.csv")
commit(repo, all = T, paste0("Update mutations per date_statistics types ",Sys.Date()))
push(repo, credentials = git.auth)