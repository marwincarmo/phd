sat <- readr::read_csv("sat_act.csv")

sat$education[1:3]
sat$SATV[4:6]

hist(sat$ACT,main = "Histogram of ACT scores",
     xlab = "ACT")

     