library(dplyr)
library(ggplot2)


plot6 <- function() {
    NEI <- readRDS('summarySCC_PM25.rds')
    SCC <- readRDS('Source_Classification_Code.rds')

    filteredSCC <- filter(SCC, grepl('on-road', ignore.case=TRUE, EI.Sector),
                          Data.Category=='Onroad')
    fipsMapping <- list(`24510`='Baltimore City', `06037`='Los Angeles')
    filteredNEI <- NEI %>%
        filter(fips %in% c('24510', '06037'), SCC %in% filteredSCC$SCC) %>%
        mutate(city=as.character(fipsMapping[fips]))

    gNEI <- group_by(filteredNEI, year, city)
    totalEmissions <- summarize(gNEI, value=sum(Emissions))

    p <- qplot(year, value, data=totalEmissions, color=city,
               geom=c('point', 'smooth'), method='lm',
               xlab='Year', ylab='Total PM25 Emissions (tons)',
               main=paste('Total PM25 Emissions in Baltimore City and Los Angeles',
                          'by year\nfor sources with EI.Sector "Mobile - On-Road"',
                          'and Data.Category "Onroad"', sep=' '))
    p <- p + coord_cartesian(ylim = c(0, 6000))
    png(filename='plot6.png', width=800, height=600, units='px')
    print(p)
    dev.off()
}
