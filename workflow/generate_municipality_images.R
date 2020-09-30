require(magick);
require(rmarkdown);
require(webshot2);

rmarkdown::render(
  input = "workflow/daily_municipality.Rmd", 
  #output_file = "reports/daily_municipality/main.html",
  output_file = "daily_municipality.html",
  output_format = "all")

screenshots = c(
  "section" = "full",
  "section.cases header" = "cases-head",
  "section.cases #section-bottom-20-van-de-week" = "cases-bottom-20",
  "section.cases main" = "cases-all",
  "section.hosp header" = "hosp-head",
  "section.hosp main" = "hosp-all",
  "section.deaths header" = "deaths-head",
  "section.deaths main" = "deaths-all"
)

for (i in seq_along(screenshots)) {
  selector <- names(screenshots)[i]
  file <- paste(getwd(), '/plots/list-', screenshots[i], '.png', sep='')
  webshot2::webshot(
    url = paste("file://", getwd(), "/workflow/daily_municipality.html", sep=''),
    file = file,
    zoom = 2,
    selector = selector, 
    expand = c(5, 5, 5, 5)
  )
  
  if (grepl("main", selector, fixed=TRUE)) {
    image.list <- image_read(file)
    image.list.width <- image_info(image.list)["width"]
    image.list.height <- image_info(image.list)["height"]
    image_crop(image.list, geometry_area(image.list.width, 2830, 0, 0)) %>%
      image_write(paste(getwd(), '/plots/list-', screenshots[i], '-part1.png', sep=''))
    
    image_crop(image.list, geometry_area(image.list.width, 3020, 0, 2840)) %>%
      image_write(paste(getwd(), '/plots/list-', screenshots[i], '-part2.png', sep=''))
    
    image_crop(image.list, geometry_area(image.list.width, image.list.height - 5875, 0, 5875)) %>%
      image_write(paste(getwd(), '/plots/list-', screenshots[i], '-part3.png', sep=''))
  }
}

rm(list=ls())
