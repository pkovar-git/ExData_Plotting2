setwd("C:/R")

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

unique(NEI$year)
[1] 1999 2002 2005 2008
plot1ds <- subset(NEI,NEI$year %in% c('1999','2002','2005','2008'))[,c("Emissions","year")]

tapply(plot1ds$Emissions,plot1ds$year,FUN=sum)

aggregate(Emissions~year,data=plot1ds,FUN=sum)


library(doBy)
summaryBy(Emissions~year,data=plot1ds,FUN=sum)

plot1dsres <- summaryBy(Emissions~year,data=plot1ds,FUN=sum)

with(plot1dsres,plot(year, Emissions.sum, type="o"))

model <- lm(year ~ Emissions.sum, plot1dsres)

abline(model, lwd = 2)
#with(plot1dsres,plot(year,Emissions.sum))

#TODO
#lables, lm muze byt zkonstruovana ze vsech dostupnych hodnot..
 
plot2ds <- subset(NEI,NEI$fips=='24510')[,c("Emissions","year")]
plot2dsres <- summaryBy(Emissions~year,data=plot2ds,FUN=sum)

with(plot2dsres,plot(year, Emissions.sum, type="o"))
model <- lm(Emissions.sum ~ year, plot2dsres)
abline(model, lwd = 2)

--names(NEI)
plot3ds <- subset(NEI,NEI$fips=='24510')[,c("Emissions","year","type")]
plot3dsres <- summaryBy(Emissions~year + type,data=plot3ds,FUN=sum)

qplot(year, Emissions.sum, data = plot3dsres, facets = . ~ type, geom = c("point", "smooth"), method = "lm")


SCC_comb_coal <- SCC[grepl('Combustion',SCC$SCC.Level.One)&grepl('Coal',SCC$SCC.Level.Three),c('SCC')]
plot4ds <- NEI[NEI$SCC %in% SCC_comb_coal,c("Emissions","year")]
plot4dsres <- summaryBy(Emissions~year,data=plot4ds,FUN=sum)

ggplot(data = plot4dsres, aes(year, Emissions.sum))+geom_line()+geom_point(size=3)+
  geom_smooth(method="lm", se=FALSE, col="steelblue") + labs(x='Year') + 
  labs(y=expression("Total Emissions" * PM[2.5])) +
  ggtitle("Emissions from coal combustion for United States")


SCC_motor_vehicle <- SCC[grepl('Mobile',SCC$EI.Sector)&grepl('Vehicles',SCC$EI.Sector)&SCC$Data.Category=='Onroad',c('SCC')]
plot5ds <- subset(NEI,NEI$fips=='24510'&NEI$SCC %in% SCC_motor_vehicle)[,c("Emissions","year")]
plot5dsres <- summaryBy(Emissions~year,data=plot5ds,FUN=sum)

plot5dsres<-transform(plot5dsres,year=factor(year))
#upresni roky, problem s tiskem

ggplot(data = plot5dsres, aes(year, Emissions.sum))+geom_line()+geom_point(size=3)+ 
labs(x='Year',y=expression("Total Emissions" * PM[2.5])) +ggtitle("Emissions from from motor vehicle in Baltimore City")

+geom_smooth(method="lm", se=FALSE, col="steelblue")

ggplot(data = plot5dsres, aes(year, Emissions.sum)) +geom_line() +geom_point(size=3)+
  geom_smooth(method="lm", se=FALSE, col="blue")+
  labs(x='Year',y="Emissions", title=expression("Annual Emissions PM "[2.5]~' from vehicel in Baltimore City'))


library(ggplot2)
plot6ds <- subset(NEI,NEI$fips %in% c('24510','06037')&NEI$SCC %in% SCC_motor_vehicle)[,c("Emissions","year","fips")]
plot6dsres <- summaryBy(Emissions~year+fips,data=plot6ds,FUN=sum)
qplot(year, Emissions.sum, data = plot6dsres, facets = . ~ fips, geom = c("point", "smooth"), method = "lm")

plot6dsres$County <- 'Los Angeles'
plot6dsres[plot6dsres$fips=='24510','County'] <- 'Baltimore'

g <- ggplot (plot6dsres, aes(year, Emissions.sum))
g + geom_point() + facet_grid(. ~ fips) +geom_line()+geom_smooth(method="lm", se=FALSE, col="blue")

g <- ggplot (plot6dsres, aes(year, Emissions.sum, group=County, color=County))
g + geom_line() + geom_point( size=3 ) + geom_smooth(method="lm", se=FALSE, col="blue")

g + geom_point() + facet_wrap(~ fips) +geom_line()+geom_smooth(method="lm", se=FALSE, col="blue")


ggplot(data=plot_6, aes(x=Year, y=Change, group=City, color=City)) + 
  geom_line() + geom_point( size=4, shape=21, fill="white") + xlab("Year") + ylab("Change in Emissions (tons)") + ggtitle("Motor Vehicle PM2.5 Emissions Changes: Baltimore vs. LA")

#g <- ggplot (plot6dsres, aes(year, Emissions.sum))
#g + geom_point() + facet_wrap(. ~ fips) +geom_line()

qplot(year, Emissions.sum, data = plot6dsres, group = County, color = County, 
      geom = c("point", "line"), ylab = expression("Total Emissions PM"[2.5]), 
      xlab = "Year", main = "Total Emissions by County")


