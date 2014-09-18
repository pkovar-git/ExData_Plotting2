# Put downloaded *.rds file to getwd()/data directory
# setwd("C:/R")
# install doBy package

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

SCC_motor_vehicle <- SCC[grepl('Mobile',SCC$EI.Sector)&grepl('Vehicles',SCC$EI.Sector)&SCC$Data.Category=='Onroad',c('SCC')]
plot5ds <- subset(NEI,NEI$fips=='24510'&NEI$SCC %in% SCC_motor_vehicle)[,c("Emissions","year")]

library(doBy)

plot5dsres <- summaryBy(Emissions~year,data=plot5ds,FUN=sum)

#plot5dsres<-transform(plot5dsres,year=factor(year))

library("ggplot2")

ggplot(data = plot5dsres, aes(year, Emissions.sum)) +geom_line() +geom_point(size=3)+
  geom_smooth(method="lm", se=FALSE, col="blue")+
  labs(x='Year',y="Emissions", title=expression("Annual Emissions PM "[2.5]~' from vehicel in Baltimore City'))


dev.copy(png, file="./data/plot5.png", width=480, height=480)
dev.off()
