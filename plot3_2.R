# Put downloaded *.rds file to getwd()/data directory
# setwd("C:/R")
# install doBy package

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

plot3ds <- subset(NEI,NEI$fips=='24510')[,c("Emissions","year","type")]

library(doBy)

plot3dsres <- summaryBy(Emissions~year + type,data=plot3ds,FUN=sum)

library("ggplot2")

qplot(year, Emissions.sum, data = plot3dsres, facets = . ~ type, geom = c("point", "smooth"), method = "lm", 
      xlab='Year',ylab="Emissions", main=expression("Annual Emissions PM "[2.5]~' in Baltimore by type'))

dev.copy(png, file="./data/plot3.png", width=640, height=480)
dev.off()

