## Download data disabled people 
disabled.people <- read.csv("https://data.rivm.nl/covid-19/COVID-19_gehandicaptenzorg.csv", sep = ";")
filename.disabledpeople.raw  <- paste0("raw-data-archive/disabled-people-per-day/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
write.csv(disabled.people, file = filename.disabledpeople.raw,row.names = F) 

filename.disabledpeople.compressed  <- paste0("data-rivm/disabled-people-per-day/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
write.csv(disabled.people, file = gzfile(filename.disabledpeople.compressed),row.names = F) 

## Download data 70+ living at home 
living.home.70plus <- read.csv("https://data.rivm.nl/covid-19/COVID-19_thuiswonend_70plus.csv", sep = ";")
filename.living.home.70plus.raw <- paste0("raw-data-archive/70plus-living-at-home-per-day/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
write.csv(living.home.70plus, file = filename.living.home.70plus.raw,row.names = F) 

filename.living.home.70plus.compressed <- paste0("data-rivm/70plus-living-at-home-per-day/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
write.csv(living.home.70plus, file = gzfile(filename.living.home.70plus.compressed),row.names = F) 

## Download behavior
behavior <- read.csv("https://data.rivm.nl/covid-19/COVID-19_gedrag.csv", sep = ";")
filename.behavior.raw <- paste0("raw-data-archive/behavior/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
write.csv(behavior, file = filename.behavior.raw,row.names = F) 

filename.behavior.compressed <- paste0("data-rivm/behavior/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
write.csv(behavior, file = gzfile(filename.behavior.compressed),row.names = F) 

## Download nursing homes

nursing.homes <- read.csv("https://data.rivm.nl/covid-19/COVID-19_verpleeghuizen.csv", sep = ";")
filename.nursinghomes.raw <- paste0("raw-data-archive/nursing-home-datasets/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
write.csv(nursing.homes, file = filename.nursinghomes.raw,row.names = F)

filename.nursinghomes.compressed <- paste0("data-rivm/nursing-homes-datasets/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
write.csv(nursing.homes, file = gzfile(filename.nursinghomes.compressed),row.names = F)


# Git
git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Daily data download ",Sys.Date()))
push(repo, credentials = git.auth)