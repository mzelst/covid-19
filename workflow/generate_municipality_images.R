require(webshot2);
require(rmarkdown);
require(magick);

rmarkdown::render(
  input = "workflow/daily_municipality.Rmd", 
  #output_file = "reports/daily_municipality/main.html",
  output_file = "main.html",
  output_format = "all")

webshot(
  paste("file://", getwd(), "/workflow/main.html", sep=''),
  paste(getwd(), "/plots/list_municipality.png", sep=''),
  zoom = 2
)

# rmdshot("workflow/daily_municipality.Rmd", "plots/list_municipality.png", delay = 5, webshot_timeout = 120)
