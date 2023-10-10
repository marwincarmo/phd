
# 01-import.R -------------------------------------------------------------
# read in and print data

# packages ----------------------------------------------------------------

library(tidyverse)

data(iris)
write_csv(iris, file = "data-raw/iris.csv")

iris <- read_csv(file = "data-raw/iris.csv")
print(iris, n = 6)
