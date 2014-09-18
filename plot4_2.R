# Put downloaded *.rds file to getwd()/data directory
# setwd("C:/R")
# install doBy package

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

SCC_comb_coal <- SCC[grepl('Combustion',SCC$SCC.Level.One)&grepl('Coal',SCC$SCC.Level.Three),c('SCC')]
plot4ds <- NEI[NEI$SCC %in% SCC_comb_coal,c("Emissions","year")]

library(doBy)

plot4dsres <- summaryBy(Emissions~year,data=plot4ds,FUN=sum)

plot4dsres$Emissions.sum <- plot4dsres$Emissions.sum / 1000

library("ggplot2")

ggplot(data = plot4dsres, aes(year, Emissions.sum)) +geom_line() +geom_point(size=3)+
  geom_smooth(method="lm", se=FALSE, col="blue")+
  labs(x='Year',y="Emissions(thou of tons)", title=expression("Annual Emissions PM "[2.5]~' from coal combustion in US'))

dev.copy(png, file="./data/plot4.png", width=480, height=480)
dev.off()
