

# 1. Import data ----------------------------------------------------------

sat_data <- read.csv("data/sat_act.csv")
sat_data

# 2. Class of dataset columns ---------------------------------------------

str(sat_data)

# 3. Mean and median of continuous variables ------------------------------

# Identify the continuous variables
continuous_variables <- c("age", "ACT", "SATV", "SATQ")

# Calculate the mean and median of each continuous variable
summary(sat_data[continuous_variables])


# 4. Standard deviation of continuous variables ---------------------------

sd(sat_data$age, na.rm = TRUE)
sd(sat_data$ACT, na.rm = TRUE)
sd(sat_data$SATV, na.rm = TRUE)
sd(sat_data$SATQ, na.rm = TRUE)

