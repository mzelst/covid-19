u <- "https://www.cbs.nl/nl-nl/reeksen/sterfte-per-week"

webpage <- read_html(u)
urls <- webpage %>%
  html_nodes("a") %>%
  html_attr(("href"))

urls <- data.frame(urls)
urls$number <- gsub("[^0-9.]", "",  urls$urls)
urls$number <- substr(urls$number, 3, 8)
urls$category <- grepl("nieuws", urls$urls, fixed = TRUE)
urls <- urls %>%
  filter(category == "TRUE") %>%
  filter(number >= 0) %>%
  mutate(year = substr(number, 1, 4)) %>%
  mutate(week = substr(number, 5, 6)) %>%
  setorder(year, week)
rm(webpage,u)
write.csv(urls, file = "data-misc/excess_mortality/links_cbs_mortality.csv",row.names = F)

## Download RIVM mortality graph

u <- "https://www.rivm.nl/monitoring-sterftecijfers-nederland"
webpage <- read_html(u)
imgsrc <- read_html(u) %>%
  html_node(xpath = '//*[@id="top"]/article/div[2]/div[2]/div/div/div/div[2]/div/div/img') %>%
  html_attr('src')

download.file(paste0("https://www.rivm.nl/",imgsrc), destfile="data-misc/excess_mortality/plots_weekly_update/sterfte_perweek_rivm.jpeg", mode="wb")