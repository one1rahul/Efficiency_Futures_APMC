# Get the column names of the data frame Spot_Future from columns 2 to 5
f <- colnames(Spot_Future[2:5])
# Get the column names of the data frame Spot_Future from columns 6 to 12
s <- colnames(Spot_Future[6:12])

# Initialize empty data frames to store results
aiclag <- NULL
tr0 <- NULL
tr1 <- NULL
mr0 <- NULL
mr1 <- NULL

# Loop through Futures and Spot column combinations
for (i in 2:5) {
  for (j in 6:12) {
  
    # Extract the columns of the data frame Spot_Future corresponding to columns 1, i, and j
    d <- na.omit(Spot_Future[,c(1,i,j)])
    
    # Compute the logarithm of columns 2 and 3 of d
    d1 <- log(d[,2:3])
    
    # Combine the date column with d1
    d2 <- cbind(d$Date, d1)
    
    # Rename the first column to "date"
    colnames(d2)[1] <- "date"
    
    # Convert d2 to a zoo object
    d2 <- as.data.frame(d2)
    d3 <- read.zoo(d2)
    
    # Use the VARselect function to select the optimal lag length for the VAR model
    lagselect1 <- VARselect(d3, lag.max = 10, type = "const")
    l = lagselect1[[1]][[1]]
    
    # Perform the Johansen cointegration test using the ca.jo function with the trace method
    # Store the results in Tcotest
    Tcotest <- ca.jo(d3, K = l, type = "trace", ecdet = "const", spec = "longrun")
    
    # Use the assign function to dynamically create variable names and assign the test results to them
    assign(paste0("Tcotest_", f[i-1] , s[j-5]), Tcotest)
    
    # Extract the test statistic for r=0 and r=1 and store them in tr0 and tr1, respectively
    ltr0 <- Tcotest@teststat[2]
    tr0 = as.data.frame(rbind(tr0, ltr0))
    ltr1 <- Tcotest@teststat[1]
    tr1 = as.data.frame(rbind(tr1, ltr1))
    
    # Perform the Johansen cointegration test using the ca.jo function with the eigen method
    # Store the results in Mcotest
    Mcotest <- ca.jo(d3, K = l, type = "eigen", ecdet = "const", spec = "longrun")
    
    # Use the assign function to dynamically create variable names and assign the test results to them
    assign(paste0("Mcotest_", f[i-1] , s[j-5]), Mcotest)
    
    # Extract the test statistic for r=0 and r=1 and store them in mr0 and mr1, respectively
    lmr0 <- Mcotest@teststat[2]
    mr0 = as.data.frame(rbind(mr0, lmr0))
    lmr1 <- Mcotest@teststat[1]
    mr1 = as.data.frame(rbind(mr1, lmr1))
  }
}

# names of future contracts and spot markets to use in the final table
fut_cont <- c("F1", "F1", "F1", "F1", "F1", "F1", "F1", "F2", "F2", "F2", "F2", "F2", "F2", "F2", "F3", "F3", "F3",
              "F3", "F3", "F3", "F3", "F4", "F4", "F4", "F4", "F4", "F4", "F4")
spot_mkt <- c("Gujarat", "Haryana", "Maharashtra", "Rajasthan", "UP", "WB", "MP",
             "Gujarat", "Haryana", "Maharashtra", "Rajasthan", "UP", "WB", "MP",
             "Gujarat", "Haryana", "Maharashtra", "Rajasthan", "UP", "WB", "MP",
             "Gujarat", "Haryana", "Maharashtra", "Rajasthan", "UP", "WB", "MP")

# combine names and test statistics into a new data frame
cointstat <- as.data.frame(cbind(fut_cont, spot_mkt, tr0, tr1, mr0, mr1))

# rename column names
colnames(cointstat)[1:6] <- c("Futures Contract", "Spot Market", "Trace statistics H0:r<=0",
                              "Trace statistics H0:r<=1", "Max statistics H0:r<=0", "Max statistics H0:r<=1")

# write results to Excel file
write_xlsx(cointstat,"cointstat.xlsx")

# remove all objects from workspace except "x" and "Spot_Future"
# rm(list=setdiff(ls(), c("x", "Spot_Future")))
