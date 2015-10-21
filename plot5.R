library(dplyr)
library(ggplot2)


plot5 <- function() {
    NEI <- readRDS('summarySCC_PM25.rds')
    SCC <- readRDS('Source_Classification_Code.rds')

    filteredSCC <- filter(SCC, grepl('on-road', ignore.case=TRUE, EI.Sector),
                          Data.Category=='Onroad')
    filteredNEI <- filter(NEI, fips == '24510', SCC %in% filteredSCC$SCC)

    gNEI <- group_by(filteredNEI, year)
    totalEmissions <- summarize(gNEI, value=sum(Emissions))

    p <- qplot(year, value, data=totalEmissions,
               geom=c('point', 'smooth'), method='lm',
               xlab='Year', ylab='Total PM25 Emissions (tons)',
               main=paste('Total PM25 Emissions in Baltimore City by year',
                          'for sources\nwith EI.Sector "Mobile - On-Road"',
                          'and Data.Category "Onroad"', sep=' '))
    p <- p + coord_cartesian(ylim = c(0, 600))
    png(filename='plot5.png', width=800, height=600, units='px')
    print(p)
    dev.off()
}
