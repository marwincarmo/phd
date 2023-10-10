
# 02-tidy.R ---------------------------------------------------------------
# reformat data from wide to long and output

iris <- pivot_longer(data = iris,
                     cols = -Species,
                     names_to = "attribute",
                     values_to = "value")
iris

file <- "data-processed/iris-long.csv"
write_csv(iris, file = file)
