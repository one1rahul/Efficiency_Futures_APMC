# Extract column names of interest from Spot_Future dataset
f <- colnames(Spot_Future[2:5]) # Futures columns
s <- colnames(Spot_Future[6:12]) # Spot columns

# Initialize empty data frames for storing results
res_corr <- NULL # Correlation coefficient
f_std <- NULL # Futures equation residuals standard deviation
s_std <- NULL # Spot equation residuals standard deviation
fhbis <- NULL # Futures share of forecast error variance
shbis <- NULL # Spot share of forecast error variance
bv_system <- NULL # Bi-variate system name

# Loop through each pair of futures and spot columns of interest
for (i in 2:5) {
  
  for (j in 6:12) {
    
    # Extract relevant columns from Spot_Future dataset and remove any rows with missing values
    d <- na.omit(Spot_Future[,c(1,i,j)])
    
    # Take logarithm of futures and spot columns
    d1 <- log(d[,2:3])
    
    # Combine date and log-transformed data into a data frame and rename columns
    d2 <- cbind(d$Date, d1)
    colnames(d2)[1] <- "date"
    d2 <- as.data.frame(d2)
    
    # Convert data frame to zoo object
    d3 <- read.zoo(d2)
    
    # Determine optimal number of lags using VARselect and estimate VECM model
    lagselect1 <- VARselect(d3, lag.max = 10, type = "const")
    l = lagselect1[[1]][[1]]
    v <- VECM(d3, l-1, r = 1, include = "const", beta = NULL, LRinclude = "none", estim = "ML")
    vs <- summary(v)
    
    # Extract relevant coefficients and statistics from VECM model summary
    fa <- as.numeric(vs[["coefMat"]][1])
    sa <- as.numeric(vs[["coefMat"]][3+l*2])
    corr_ro <- as.numeric(cor(vs[["residuals"]])[1,2]) # correlation coefficient
    f_sd <- as.numeric((vs[["sigma"]][1,1])^0.5)  ## futures eq. residuals standard deviation
    s_sd <- as.numeric((vs[["sigma"]][2,2])^0.5)  ## spot eq. residuals standard deviation
    
    # Calculate futures and spot equation shares of forecast error variance
    f_sai = (sa)/(sa-fa)
    s_sai = (fa)/(sa-fa)
    d_1 = ((f_sai * f_sd) + (s_sai * corr_ro * s_sd))^2
    d_2 = (s_sai^2) * (s_sd^2) * (1 - corr_ro^2)
    f_IS = d_1/(d_1 + d_2)
    s_IS = d_2/(d_1 + d_2)
    
    # Append results to respective data frames
    res_corr = as.data.frame(rbind(res_corr, corr_ro))
    f_std = as.data.frame(rbind(f_std, f_sd))
    s_std = as.data.frame(rbind(s_std, s_sd))
    fhbis = as.data.frame(rbind(fhbis, f_IS))
    shbis = as.data.frame(rbind(shbis, s_IS))
    
    # Combine futures and spot column names to create bi-variate system name  
    sys <- paste0(f[i-1], "_" ,s[j-5])
    bv_system = as.data.frame(rbind(bv_system, sys))
  }
}

# combine names and test statistics into a new data frame
Inf_share <- as.data.frame(cbind(bv_system, fhbis, shbis))

# rename column names
colnames(Inf_share) <- c("BiVariateSystem", "FuturesShare", "SpotShare")

# write results to Excel file
write_xlsx(std_dev,"std_dev.xlsx")

# remove all objects from workspace except "x" and "Spot_Future"
# rm(list=setdiff(ls(), c("x", "Spot_Future")))
