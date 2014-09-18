# Put downloaded *.rds file to getwd()/data directory
# setwd("C:/R")
# install doBy package

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

plot1ds <- subset(NEI,NEI$year %in% c('1999','2002','2005','2008'))[,c("Emissions","year")]

library(doBy)

plot1dsres <- summaryBy(Emissions~year,data=plot1ds,FUN=sum)

plot1dsres$Emissions.sum <- plot1dsres$Emissions.sum / 1000
  
with(plot1dsres,plot(year,Emissions.sum,type="o",main=expression("Total Emissions PM in US"[2.5]~' in US'),
                     xlab="Year",ylab="Emissions (thou of tons)",xaxt='n'))

axis(side=1, at=c("1999", "2002", "2005", "2008"))

model <- lm(Emissions.sum ~ year, plot1dsres)
abline(model, lwd = 2, col= "blue")

dev.copy(png, file="./data/plot1.png", width=480, height=480)
dev.off()



