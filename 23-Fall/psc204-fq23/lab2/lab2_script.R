# Week 2 lab

file.choose()

# loading a file

data_week2 <- read.csv("C:\\Users\\marwi\\OneDrive\\Documentos\\Rprojects\\phd\\23-Fall\\psc204-fc23\\lab2\\data\\data_lab_week2.csv")

getwd()

colnames(data_week2)
head(data_week2)

data_week2[, 2]

# specific subject we don't know the row

data_week2[data_week2$Subject == "M011", ]

data_week2[data_week2$Sex == "Male" & 
             data_week2$Age > 80, ]

class(data_week2$Subject)

str(data_week2)


data_week2$id <- 1:26

data_week2$`20` <- 20

head(data_week2)

mean(data_week2[data_week2$Age > 100, "Consolability"])

?sd
