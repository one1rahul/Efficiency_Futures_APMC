# list of packages to be loaded
x <- c("tsDyn", "urca", "vars", "FinTS", "rugarch", "rmgarch", "writexl", "fBasics")

# loading packages
lapply(x, require, character.only = TRUE)

# initialize empty data frames to store results
critical <- NULL
t_stat <- NULL
critical2 <- NULL
t_stat2 <- NULL

# loop through columns 2 to 12 of Spot_Future data frame
for (i in 2:12) {
  # create a new data frame with columns 1 and i and remove missing values
  d <- na.omit(Spot_Future[,c(1,i)])
  
  # take the log of column i
  d1 <- log(d[,2])
  
  # combine date and d1 columns into a new data frame
  d2 <- cbind(d$Date, d1)
  colnames(d2)[1] <- "date"
  d2 <- as.data.frame(d2)
  
  # convert data frame to zoo object
  d3 <- read.zoo(d2)
  
  # perform ADF test on level with intercept and select optimal lag using AIC
  r1 <- ur.df(d3, type = "drift", selectlags = "AIC")
  
  # get critical values and test statistics at 1% significance level
  crt1 <- r1@cval[1]
  critical = as.data.frame(rbind(critical, crt1))
  tst1 <- r1@teststat[1]
  t_stat = as.data.frame(rbind(t_stat, tst1))
  
  # perform ADF test on first difference with intercept and select optimal lag using AIC
  r2 <- ur.df(diff(d3), type = "drift", selectlags = "AIC")
  
  # get critical values and test statistics at 1% significance level
  crt2 <- r2@cval[1]
  critical2 = as.data.frame(rbind(critical2, crt2))
  tst2 <- r2@teststat[1]
  t_stat2 = as.data.frame(rbind(t_stat2, tst2))
}

# get column names of columns 2 to 12 of Spot_Future data frame
series <- colnames(Spot_Future[2:12])

# combine series names, critical values, and test statistics into a new data frame
unit_root_stat <- as.data.frame(cbind(series, critical, t_stat, t_stat2))

# rename column names
colnames(unit_root_stat)[1:4] <- c("Series", "1% Critical Value", "At Level", "At First Difference")

# write results to Excel file
write_xlsx(unit_root_stat,"unit_root_stat.xlsx")

# remove all objects from workspace except "x" and "Spot_Future"
## rm(list=setdiff(ls(), c("x", "Spot_Future")))
