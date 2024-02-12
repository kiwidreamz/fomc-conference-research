library(ggplot2)

file_path <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/interest rates/old/FEDFUNDS.csv"
fed_funds <- read.csv(file_path)
file_path <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/interest rates/old/NASDAQCOM.csv"
daq <- read.csv(file_path)

fed_funds$DATE <- as.Date(fed_funds$DATE)
daq$DATE <- as.Date(daq$DATE)

plot(daq$DATE, daq$NASDAQCOM, type = "l", col = "deepskyblue3", lwd = 2, xlab = "Date", ylab = "NASDAQCOM", main = "Nasdaq Composite vs Fed Funds")
lines(fed_funds$DATE, fed_funds$FEDFUNDS, col = "forestgreen", lwd = 2)


par(mfrow = c(2, 1))
plot(daq$DATE, daq$NASDAQCOM, type = "l", col = "deepskyblue3", lwd = 2, 
     xlab = "Date", ylab = "NASDAQCOM", main = "Nasdaq Composite")
plot(fed_funds$DATE, fed_funds$FEDFUNDS, type = "l", col = "forestgreen", lwd = 2,
     xlab = "Date", ylab = "Fed Funds", main = "Fed Funds")
par(mfrow = c(1, 1))
