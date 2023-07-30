# A character vector of package names
x <- c("tsDyn", "urca", "vars", "FinTS", "rugarch", "rmgarch", "writexl", "fBasics")

install.packages(x)

# Load packages using `lapply` function
lapply(x, require, character.only = TRUE)

# Create empty data frames to store resutls
m <- NULL
std <- NULL
sk <- NULL
ks <- NULL
obs <- NULL
jb <- NULL
bp <- NULL

# Loop through columns 2 to 12 of the `Spot_Future` data frame
for (i in 2:12) {
  
  # Subset the `Spot_Future` data frame to columns 1 and i, remove missing values, and convert to zoo object
  d <- na.omit(Spot_Future[,c(1,i)])
  d1 <- read.zoo(d)
  
  # Calculate the mean, standard deviation, skewness, kurtosis, number of observations, Jarque-Bera statistic and p-value, 
  # and Box-Pierce statistic and p-value for the subsetted data frame
  m1 <- mean(d1)
  m = as.data.frame(rbind(m, m1))
  std1 <- sd(d1)
  std = as.data.frame(rbind(std, std1))
  sk1 <- skewness(d1, method = "moment")
  sk2 <- as.numeric(sk1)
  sk = as.data.frame(rbind(sk, sk2))
  ks1 <- kurtosis(d1, method = "moment")
  ks2 <- as.numeric(ks1)
  ks = as.data.frame(rbind(ks, ks1))
  obs1 <- nrow(d)
  obs = as.data.frame(rbind(obs, obs1))
  jb1 <- jarqueberaTest(d1)
  jb2 <- as.data.frame(cbind(jb1@test[["statistic"]], jb1@test[["p.value"]]))
  jb = as.data.frame(rbind(jb, jb2))
  bp1 <- Box.test(d1, lag = 10, type = "Box-Pierce")
  bp2 <- as.data.frame(cbind(bp1[["statistic"]], bp1[["p.value"]]))
  bp <- as.data.frame(rbind(bp, bp2))
}

# Calculate the coefficient of variation, and create a data frame to store descriptive statistics
cv <- std/m

# get column names of columns 2 to 12 of Spot_Future data frame
series <- colnames(Spot_Future[2:12])

# combine series names, number of observations, mean, standard deviation, coefficient of variation, skewness, kurtosis, 
# Jarque-Bera statistic and pvalue, and Box-Pierce statistic and pvalue into a new data frame
desc_stat <- as.data.frame(cbind(series, obs, m, std, cv, sk, ks, jb, bp))

# rename column names
colnames(desc_stat)[1:11] <- c("Series", "Observations", "Mean", "Std. Dev.", "CV", "Skewness", "Kurtosis",
    "JB_statistic", "JB_pvalue", "Q(10)_statistic", "Q(10)_pvlaue")

# Write the descriptive statistics to an Excel file
write_xlsx(desc_stat,"desc_stat.xlsx")

# Clean up the environment by removing all objects except for `x` and `Spot_Future`
# rm(list=setdiff(ls(), c("x", "Spot_Future")))
