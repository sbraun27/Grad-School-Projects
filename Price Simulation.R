####################################################################
# Author: Steven Braun
# Course: Financial Modeling - FIN 4500
# Topic: Price Simulation
# University of Denver
####################################################################


# Load required or helpful packages

library(dplyr) 
library(data.table)
library(fBasics)
library(fPortfolio)
library(tseries)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(quantmod)
library(xts)


# Set basic options
options(scipen = 999)
set.seed(2019)

# Set Working Directory
setwd("C://Classes/Fin 4500")


# Get stock prices and store them
getSymbols("F", from = "2014-03-04", to = "2019-03-04", src = "yahoo")
ford.prices <- F

# get daily stock return
ford.returns <- dailyReturn(ford.prices)
colnames(ford.returns) <- "Ford_Returns"

# Plot daily prices
plot.xts(ford.prices$F.Adjusted, main = "Ford Prices")

# Plot daily returns
plot.xts(ford.returns, main = "Ford Daily Returns")


# Plot the distribution of daily returns
return.den <- density(ford.returns$Ford_Returns)
plot(return.den, main = "Distribution of Ford Stock Returns")
polygon(return.den, col = "tomato")

# Plot the distribution of daily stock prices
prices.den <- density(ford.prices$F.Adjusted)
plot(prices.den, main = "Distribution of Ford Stock Prices")
polygon(prices.den, col = "blue")


# Find the last price, and save it to p0
num.obs <- dim(ford.prices)[1]
p0 <- ford.prices[num.obs, 6]

# set the change in time
deltat <- 1/250

# Set the average
mu <- mean(ford.returns)*250

# Set the standard deviation
sigma <- sd(ford.returns)*sqrt(250)

# generate 250 random numbers given mean of 0 and sd of 1
z <- rnorm(n= 250, mean = 0, sd = 1)

# make an empty data frame with two columns and change the column names to time and price
sim.prices.matrix <- as.data.frame(matrix(NA, nrow = 251, ncol = 2))
colnames(sim.prices.matrix) <- c("Time", "Price")

# initialize starting values. Time = 0 and Price equals last price
sim.prices.matrix[1, 1] <- 0
sim.prices.matrix[1, 2] <- p0

# for loop to generate a random walk of prices
for (i in 1:250) {
  sim.prices.matrix[i + 1, 1] <- i
  sim.prices.matrix[i + 1, 2] <- (sim.prices.matrix[i, 2]*exp(mu*deltat+sigma*z[i]*sqrt(deltat)))
}

# Plot Price over time, then a histogram of the prices
plot(sim.prices.matrix$Time, sim.prices.matrix$Price, main = "Simulated Price for Ford", type = "l", xlab = "Days", ylab = "Price")
hist(sim.prices.matrix$Price, xlab = "Prices", ylab= "Frequency", main = "Simulated Ford Price Distribution")

# Plot the density of the simulated prices
sim.dens <- density(sim.prices.matrix$Price)
plot(sim.dens, main = "Simulated Price Density")
polygon(sim.dens, col = "blue")

####### Redo simulation with new parameters

mu2 <- 0.00
sigma2 <- .15
z2 <- rnorm(n = 250, mean = 0, sd = 1)

sim.prices.matrix2 <- as.data.frame(matrix(NA, nrow = 251, ncol = 2))
colnames(sim.prices.matrix2) <- c("Time", "Price")

sim.prices.matrix2[1, 1] <- 0
sim.prices.matrix2[1, 2] <- p0

for (i in 1:250) {
  sim.prices.matrix2[i + 1, 1] <- i
  sim.prices.matrix2[i + 1, 2] <- (sim.prices.matrix2[i, 2]*exp(mu2*deltat + sigma2*z2[i]*sqrt(deltat)))
}

# Find the minimum and maximum (+- 1) fo the simulated prices
yminimum <- min(sim.prices.matrix$Price, sim.prices.matrix2$Price) - 1
ymaximum <- max(sim.prices.matrix$Price, sim.prices.matrix2$Price) + 1

# Plot the two simulations in the same plot
plot(sim.prices.matrix$Time, sim.prices.matrix$Price, main = "Simulated Prices of Stock", type = "l", xlab = "days", ylab = "Price", col = "blue", ylim = c(yminimum, ymaximum))
lines(sim.prices.matrix2$Time, sim.prices.matrix2$Price, col = "red", lty = 3)
legend("bottomleft", legend = c("First Simulation", "Second Simulation"), lty = c(1,3), col = c("blue", "red"))












