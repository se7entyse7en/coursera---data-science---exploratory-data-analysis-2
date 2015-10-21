library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)


plot4 <- function() {
    NEI <- readRDS('summarySCC_PM25.rds')
    SCC <- readRDS('Source_Classification_Code.rds')

    filteredSCC <- filter(SCC, grepl('coal', ignore.case=TRUE, Short.Name))
    filteredNEI <- filter(NEI, SCC %in% filteredSCC$SCC)

    gNEI <- group_by(filteredNEI, year, fips, type)
    totalEmissions <- summarize(gNEI, value=sum(Emissions))
    p1 <- qplot(year, value, data=totalEmissions, color=I('dodgerblue4'),
                alpha=I(0.1), geom=c('point', 'smooth'), method='lm',
                xlab='Year', ylab='Total PM25 Emissions (tons)',
                main=paste('Total PM25 Emissions from coal combustion-related',
                           'sources by year', sep=' '))
    p1 <- p1 + coord_cartesian(ylim = c(0, 30000))
    p2 <- qplot(year, value, data=totalEmissions, color=I('dodgerblue4'),
                alpha=I(0.1), geom=c('point', 'smooth'), method='lm',
                xlab='Year', ylab='Total PM25 Emissions (tons)',
                main=paste('Total PM25 Emissions from coal combustion-related',
                           'sources by year\n zoomed on regression line', sep=' '))
    p2 <- p2 + coord_cartesian(ylim = c(0, 250))
    p3 <- qplot(year, value, data=totalEmissions, color=I('dodgerblue4'),
                alpha=I(0.1), facets=. ~ type,
                xlab='Year', ylab='Total PM25 Emissions (tons)',
                main=paste('Total PM25 Emissions from coal combustion-related',
                           'sources by year and type', sep=' '))
    p3 <- p3 + coord_cartesian(ylim = c(0, 30000))

    gNEI <- group_by(filteredNEI, year, fips)
    totalEmissions <- summarize(gNEI, value=sum(Emissions))
    totalEmissionsDiff <- totalEmissions %>%
        spread(year, value) %>%
        mutate(start=`1999`, end=`2008`, diff=end-start) %>%
        select(fips, start, end, diff) %>%
        filter(!is.na(diff)) %>%
        arrange(desc(diff))

    mostIncreaseFips <- head(totalEmissionsDiff, 10)$fips
    mostDecreaseFips <- tail(totalEmissionsDiff, 10)$fips

    mostIncrease <- totalEmissions %>%
        filter(fips %in% mostIncreaseFips) %>%
        mutate(fips=factor(fips, levels=mostIncreaseFips),
               group='top 10 increase') %>%
        arrange(fips)
    mostDecrease <- totalEmissions %>%
        filter(fips %in% mostDecreaseFips) %>%
        mutate(fips=factor(fips, levels=mostDecreaseFips),
               group='top 10 decrease') %>%
        arrange(fips)
    mostDiff <- rbind(mostIncrease, mostDecrease)
    mostDiff$group <- factor(mostDiff$group, levels=c('top 10 increase', 'top 10 decrease'))
    p4 <- qplot(year, value, data=mostDiff, facets=. ~ group, color=fips,
                geom=c('point', 'smooth'), method='lm', se=FALSE,
                xlab='Year', ylab='Total PM25 Emissions (tons)',
                main=paste('Total PM25 Emissions from coal combustion-related',
                           'sources by year\nfor the counties with the highest',
                           'difference', sep=' '))
    p4 <- p4 + coord_cartesian(ylim = c(0, 30000))
    png(filename='plot4.png', width=1200, height=800, units='px')
    grid.arrange(p1, p3, p2, p4, nrow=2, ncol=2)
    dev.off()
}
