rmarkdown::render(
  input = "workflow/daily_municipality.Rmd", 
  #output_file = "reports/daily_municipality/main.html",
  output_file = "daily_municipality.html",
  output_format = "all")

screenshots = c(
  "article.cases > header" = "cases-head",
  "article.cases #section-hoogste-besmettingsgraad" = "cases-bottom-20",
  "article.cases section.municipalities-a-g" = "cases-all-part1",
  "article.cases section.municipalities-h-p" = "cases-all-part2",
  "article.cases section.municipalities-r-z" = "cases-all-part3",
  "article.hosp > header" = "hosp-head",
  "article.hosp section.municipalities-a-g" = "hosp-all-part1",
  "article.hosp section.municipalities-h-p" = "hosp-all-part2",
  "article.hosp section.municipalities-r-z" = "hosp-all-part3",
  "article.deaths > header" = "deaths-head",
  "article.deaths section.municipalities-a-g" = "deaths-all-part1",
  "article.deaths section.municipalities-h-p" = "deaths-all-part2",
  "article.deaths section.municipalities-r-z" = "deaths-all-part3"
)

for (i in seq_along(screenshots)) {
  selector <- names(screenshots)[i]
  file <- paste(getwd(), '/plots/list-', screenshots[i], '.png', sep='')
  webshot2::webshot(
    url = paste("file://", getwd(), "/workflow/daily_municipality.html", sep=''),
    file = file,
    zoom = 3,
    selector = selector, 
    expand = c(5, 5, 5, 5)
  )
}

