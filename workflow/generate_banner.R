require(magick);
require(stringr);

Sys.setlocale("LC_TIME", "nl_NL")
date <- Sys.Date() %>%
  format('%d %B %Y') %>%
  str_to_title()  %>%
  str_replace( '^0', '')

banner <- image_read('banners/template.png') %>%
  image_annotate( date, font = 'Cabin', weight = '600', size = 40,  location = "+0+80", gravity = "center")

image_write(banner, str_c("banners/",Sys.Date(),".png") )
rm(banner,date)
