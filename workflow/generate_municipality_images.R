require(webshot2)
require(rmarkdown)

rmarkdown::render(
  input = "workflow/daily_municipality_test.Rmd", 
  #output_file = "reports/daily_municipality/main.html",
  output_file = "main.html",
  output_format = "all")
#rmdshot("workflow/daily_municipality_test.Rmd", "plots/list_municipality.png", delay = 60, webshot_timeout = 120)
