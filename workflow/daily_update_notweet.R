require(cowplot)
require(tidyverse)
require(rjson)
require(data.table)

rivm.data <- read.csv("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv", sep=";") ## Read in data with all cases until today

# Parse RIVM, NICE and corrections data
source("workflow/parse_rivm-data.R") ## Run only after new data upload by RIVM at 14:15
source("workflow/parse_municipalities.R")
source("workflow/parse_nice-data.R")
source("workflow/parse_corrections.R")

## Merge RIVM, NICE and corrections data

rivm_by_day <- read.csv("data/rivm_by_day.csv")
nice_today <- read.csv("data-nice/nice-today.csv")
corrections.perday <- read.csv("corrections/corrections_perday.csv")
daily_datalist <- list(rivm_by_day,nice_today,corrections.perday)

all.data <- Reduce(
  function(x, y, ...) merge(x, y, by="date",all.x = TRUE, ...),
  daily_datalist
)

all.data$date <- as.Date(all.data$date)
all.data <- all.data[order(all.data$date),]

all.data$positive_7daverage <- round(frollmean(all.data[,"new.infection"],7),0) # Calculate 7-day average (based on newly reported infections, gross number)

write.csv(all.data, file = "data/all_data.csv")

source("plot_scripts/daily_plots.R")
source("plot_scripts/daily_maps_plots.R")

all.data <- read.csv("data/all_data.csv")
nice_by_day <- read.csv("data/nice_by_day.csv")
