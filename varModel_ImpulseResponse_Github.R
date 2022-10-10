
# library(vars)
# Implementing VAR model - estimating/ fitting params

set.seed(123) # Reset random number generator for reasons of reproducibility

# Generate sample
t <- 200 # Number of time series observations
k <- 2 # Number of endogenous variables == independent vars
p <- 2 # Number of lags == max no. lags? --> so setting constraints

# Generate coefficient matrices
# This approach is one way of making TRACTABLE + helping
# the coder focus on lags separately/ in stages

A.1 <- matrix(c(-.3, .6, -.4, .5), k) # Coefficient matrix of lag 1
# --> this sets one part of the simultaneous equations
A.2 <- matrix(c(-.1, -.2, .1, .05), k) # Coefficient matrix of lag 2
# --> This sets the 2nd order parts of the simultaneous eqns

# More detail re. order of lags!!
# So IF had specified 4 lags, would need to define these lags in each simulatenous eqn
# ...it may be the case that only 1 of the 17 eqns in the matrix has a 4th order lag
# with another dependent variable!!

# Now make sense why BIND the lag matrices together == coefficient matrices
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices

# Generate series
#                data, nrow, ncol
series <- matrix(0, k, t + 2*p) # Raw series with zeros

# suspect %*% is MATRIX MULTIPLICATION
for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,0.5)
  series[, i] <- A.1%*%series[, i-1] + A.2%*%series[, i-2] + rnorm(k, 0, .5)
}

# So now have 2 series == the dependent variables whose lags
# being used in this VAR model. Both series/ time series contained in "serie" object

series <- ts(t(series[, -(1:p)])) # Convert to time series format
names <- c("V1", "V2") # Rename variables
plot.ts(series) # Plot the series


### Fitting/ estimating VAR model

library(vars) # Load package
# HELP documentation states that VAR estimation utilitses OLS per eqn!

## VAR with trend, VAR with const -- suppose this depends on the FORM of the DEPENDENT
# variable == the endogenous variable

# CONJECTURE
# The series DEPENDENT/ endogenous variables here clearly have 0 MEAN + no trend
# and no clear seasonality AND level is at 0, so no need for a CONST component to add to all results!!

# VAR function documentation VERY CLEAR
var.1 <- VAR(series, 2, type = "none") # Estimate the model

# CENTRAL ISSUE of VAR analysis - finding no. lags --> best results
var.aic <- VAR(series, type = "none", lag.max = 5, ic = "AIC")

# Create another VAR here, with different no. lags specified and
# INFO MEASURE of AIC specified here
# Here, automating MODEL selection more. Do no set explicity lags
# Allow model selection based on AIC of the models

summary(var.aic)
# helpfully, can see STANDARD ERRORS etc.
# OLS-type interpretation of coefficients!!

# Extract coefficients, standard errors etc. from the object produced by the VAR function
est_coefs <- coef(var.aic)
est_coefs


# Extract only the coefficients for eqns for both dependent variables
# and combine them to a single matrix/ table
est_coefs <- rbind(est_coefs[[1]][, 1], est_coefs[[2]][, 1]) 

# Print the rounded estimates in the console
round(est_coefs, 2)

# Calculate the IRF - shock series with 1 std shock + view response in series 2
ir.1 <- irf(var.1, impulse = "Series.1", response = "Series.2", n.ahead = 20, ortho = FALSE)
# Plot the IRF
plot(ir.1)

# CUMULATIVE: Calculate impulse response - with CONFIDENCE INTERVALS
ir.2 <- irf(var.1,impulse="Series.1",response="Series.2",n.ahead = 20,ortho = FALSE,
            cumulative = TRUE)
# Plot
plot(ir.2)



