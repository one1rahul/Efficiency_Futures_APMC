# Get the column names for Futures and Spot data frames
f <- colnames(Spot_Future[2:5])
s <- colnames(Spot_Future[6:12])

# Initialize empty data frames to store results
fadj <- NULL
fadjpval <- NULL
sadj <- NULL
sadjpavl <- NULL
bv_system <- NULL

# Loop through Futures and Spot column combinations
for (i in 2:5) {
  for (j in 6:12) {
    
    # Extract data and prepare for VECM
    d <- na.omit(Spot_Future[,c(1,i,j)])
    d1 <- log(d[,2:3])
    d2 <- cbind(d$Date, d1)
    colnames(d2)[1] <- "date"
    d2 <- as.data.frame(d2)
    d3 <- read.zoo(d2)
    
    # Select lag and estimate VECM
    lagselect1 <- VARselect(d3, lag.max = 10, type = "const")
    l = lagselect1[[1]][[1]]
    v <- VECM(d3, l-1, r = 1, include = "const", beta = NULL, LRinclude = "none", estim = "ML")
    vs <- summary(v)
    
    # Store results in respective data frames
    assign(paste0("vs_", f[i-1] , s[j-5]), vs)
    fa <- vs[["coefMat"]][1]  # futures equation adjustment Coefficient
    fadj = as.data.frame(rbind(fadj, fa))
    fap <- vs[["coefMat"]][1,4] # p values of futures equation adjustment coeff.
    fadjpval = as.data.frame(rbind(fadjpval, fap))
    sa <- vs[["coefMat"]][3+l*2] # spot equation adjustment Coefficient
    sadj = as.data.frame(rbind(sadj, sa))
    sap <- vs[["coefMat"]][3+l*2,4] # p values of spot equation adjustment coeff.
    sadjpavl = as.data.frame(rbind(sadjpavl, sap))
    sys <- paste0(f[i-1], "_" ,s[j-5]) # name of the bivariate system
    bv_system = as.data.frame(rbind(bv_system, sys))
  }
}

# Combine results into a single data frame and write to Excel
adjcoeff <- as.data.frame(cbind(bv_system ,fadj, fadjpval, sadj, sadjpavl))

# rename column names
colnames(adjcoeff) <- c("BiVariateSystem", "FuturesAdjCoeff", "F_Pvalue", "SpotAdjCoeff", "S_Pvalue")

# write results to Excel file
write_xlsx(adjcoeff,"adjcoeff.xlsx")

# remove all objects from workspace except "x" and "Spot_Future"
# rm(list=setdiff(ls(), c("x", "Spot_Future")))
