# Put downloaded *.rds file to getwd()/data directory
# setwd("C:/R")
# install doBy package

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

plot2ds <- subset(NEI,NEI$fips=='24510')[,c("Emissions","year")]

library(doBy)

plot2dsres <- summaryBy(Emissions~year,data=plot2ds,FUN=sum)

with(plot2dsres,plot(year, Emissions.sum, type="o",main=expression("Annual Emissions PM "[2.5]~' in Baltimore'),
                     xlab="Year",ylab="Emissions",xaxt='n'))

axis(side=1, at=c("1999", "2002", "2005", "2008"))

model <- lm(Emissions.sum ~ year, plot2dsres)

abline(model, lwd = 2, col="blue")

dev.copy(png, file="./data/plot2.png", width=480, height=480)
dev.off()
