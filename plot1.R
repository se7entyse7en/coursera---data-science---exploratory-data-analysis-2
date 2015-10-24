library(dplyr)


plot1 <- function() {
    NEI <- readRDS('summarySCC_PM25.rds')
    SCC <- readRDS('Source_Classification_Code.rds')

    gNEI <- group_by(NEI, year)
    totalEmissions <- summarize(gNEI, value=sum(Emissions))

    png(filename='plot1.png', width=480, height=480, units='px')
    with(totalEmissions, {
        plot(year, value, pch=16, col='blue',
             xlab='Year', ylab='Total PM25 Emissions (tons)')
        abline(lm(value~year), col='green', lwd=3)
        title(main='Total PM25 Emissions by year')
        legend(x='topright',
               legend=c('PM25 Emissions', 'Linear regression'),
               lty=c('blank', 'solid'), pch=c(16, NA), col=c('blue', 'green'))
    })
    dev.off()
}
