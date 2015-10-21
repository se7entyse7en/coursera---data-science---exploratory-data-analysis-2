library(dplyr)
library(ggplot2)


plot3 <- function() {
    NEI <- readRDS('summarySCC_PM25.rds')
    SCC <- readRDS('Source_Classification_Code.rds')

    filteredNEI <- filter(NEI, fips == '24510')
    gNEI <- group_by(filteredNEI, year, type)
    totalEmissions <- summarize(gNEI, value=sum(Emissions))

    p <- qplot(year, value, data=totalEmissions, color=type,
               facets=. ~ type, geom=c('point', 'smooth'), method='lm',
               xlab='Year', ylab='Total PM25 Emissions (tons)',
               main='Total PM25 Emissions in Baltimore City by year')
    p <- p + coord_cartesian(ylim = c(0, 3000))
    png(filename='plot3.png', width=800, height=600, units='px')
    print(p)
    dev.off()
}
