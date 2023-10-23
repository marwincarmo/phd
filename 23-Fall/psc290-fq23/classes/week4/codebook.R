# set the path
wd <- "https://github.com/emoriebeck/psc290-data-FQ23/raw/main/04-workshops/04-week4-readr"

download.file(
  url      = sprintf("%s//codebook.xlsx", wd), 
  destfile = "codebook.xlsx"
)
