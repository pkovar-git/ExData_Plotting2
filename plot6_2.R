# Put downloaded *.rds file to getwd()/data directory
# setwd("C:/R")
# install doBy package

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

SCC_motor_vehicle <- SCC[grepl('Mobile',SCC$EI.Sector)&grepl('Vehicles',SCC$EI.Sector)&SCC$Data.Category=='Onroad',c('SCC')]
plot6ds <- subset(NEI,NEI$fips %in% c('24510','06037')&NEI$SCC %in% SCC_motor_vehicle)[,c("Emissions","year","fips")]

library(doBy)

plot6dsres <- summaryBy(Emissions~year+fips,data=plot6ds,FUN=sum)
plot6dsres$County <- 'Los Angeles'
plot6dsres[plot6dsres$fips=='24510','County'] <- 'Baltimore'

library("ggplot2")

g <- ggplot (plot6dsres, aes(year, Emissions.sum, group=County, color=County))
g + geom_line() + geom_point( size=3 ) + geom_smooth(method="lm", se=FALSE, col="blue") +
    labs(x='Year',y="Emissions", title=expression("Motor Vehicle PM"[2.5]~" Emissions Changes: Baltimore vs. LA"))

dev.copy(png, file="./data/plot6.png", width=480, height=480)
dev.off()


