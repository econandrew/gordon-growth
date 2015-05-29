library(reshape2)
library(gdata)
library(ggplot2)
library(lubridate)
library(gridExtra)

setwd('~/Documents/Ideas/Blog/Gordon/')

# USA BEA GDP data from 1929 to today, ?2005 prices
gdp.bea <- read.csv('nipa116.csv',strip.white=T)
gdp.bea <- gdp.bea[gdp.bea$Measure=="Gross domestic product",c(-1)]
gdp.bea <- melt(gdp.bea, id="Measure",value.name="gdp")
gdp.bea <- gdp.bea[,c(-1)]
colnames(gdp.bea) <- c("year", "gdpagg")
gdp.bea$year <- as.numeric(sub("X","",gdp.bea$year))
gdp.bea$gdpagg <- as.numeric(gdp.bea$gdpagg)

pop.usa <- read.csv("http://cdn.rawgit.com/econandrew/gitstats/master/usapop/usapop.csv")
pop.usa$year <- year(as.Date(pop.usa$date))

gdp.bea <- merge(gdp.bea, pop.usa[,c("year","pop")])
gdp.bea$gdp <- gdp.bea$gdpagg / gdp.bea$pop * 1e9
gdp.bea <- gdp.bea[,c("year","gdp")]

# Maddisons UK GDP data to the start of time, 1990 prices
gdp.mad <- read.xls('mpd_2013-01.xlsx', sheet=1, skip=2)
colnames(gdp.mad)[1] <- "year"
gdp.mad.uk <- gdp.mad[,c("year", "England.GB.UK")]
gdp.mad.uk <- gdp.mad.uk[complete.cases(gdp.mad.uk),]
colnames(gdp.mad.uk) <- c("year", "gdp")

gdp.mad.us <- gdp.mad[,c("year", "USA")]
gdp.mad.us <- gdp.mad.us[complete.cases(gdp.mad.us),]
colnames(gdp.mad.us) <- c("year", "gdp")


# Broadberry's England and GB GDP data from 1300 to 1870
gdp.bb <- read.csv('broadberry.csv')
gdp.bb.gb <- gdp.bb[,c("year", "Great.Britain")]
gdp.bb.gb <- gdp.bb.gb[complete.cases(gdp.bb.gb),]
colnames(gdp.bb.gb) <- c("year", "gdp")

gdp.bb.en <- gdp.bb[,c("year", "England")]
gdp.bb.en <- gdp.bb.en[complete.cases(gdp.bb.en),]
colnames(gdp.bb.en) <- c("year", "gdp")

# Gordon's franken-GDP per capita
# data regimes
gdp.gor.1 <- gdp.bb.en[gdp.bb.en$year %in% 1300:1700,]
gdp.gor.2 <- gdp.bb.gb[gdp.bb.gb$year %in% 1700:1870,]
gdp.gor.3 <- gdp.mad.uk[gdp.mad.uk$year %in% 1870:1906,]
gdp.gor.4 <- gdp.mad.us[gdp.mad.us$year %in% 1906:1929,]
gdp.gor.5 <- gdp.bea[gdp.bea$year %in% 1929:2015,]


# "ratio-adjusted"
gdp.gor.4$gdp <- gdp.gor.4$gdp * (gdp.gor.5$gdp[gdp.gor.5$year == 1929] / gdp.gor.4$gdp[gdp.gor.4$year == 1929])
gdp.gor.3$gdp <- gdp.gor.3$gdp * (gdp.gor.4$gdp[gdp.gor.4$year == 1906] / gdp.gor.3$gdp[gdp.gor.3$year == 1906])
gdp.gor.2$gdp <- gdp.gor.2$gdp * (gdp.gor.3$gdp[gdp.gor.3$year == 1870] / gdp.gor.2$gdp[gdp.gor.2$year == 1870])
gdp.gor.1$gdp <- gdp.gor.1$gdp * (gdp.gor.2$gdp[gdp.gor.2$year == 1700] / gdp.gor.1$gdp[gdp.gor.1$year == 1700])
gdp.gor <- rbind(gdp.gor.1, gdp.gor.2[-1,], gdp.gor.3[-1,], gdp.gor.4[-1,], gdp.gor.5[-1,])

calc_regimes <- function(gdp, regimes, regimes.color) {
  # Geometrically interpolate to annual data
  gdp$log.gdp <- log(gdp$gdp)
  gdpint <- approx(gdp$year, gdp$log.gdp, xout=min(gdp$year):max(gdp$year))
  gdpint <- data.frame(year = gdpint$x, log.gdp = gdpint$y)
  gdpint$gdp <- exp(gdpint$log.gdp)
  
  gdpint$growth <- c(NA, exp(diff(gdpint$log.gdp))-1)*100

  # growth regimes
  regimes <- regimes[regimes %in% gdpint$year]
  regimes.growth <- unlist(sapply(1:length(regimes)-1, function(reg) {
    start <- regimes[reg]
    end <- regimes[reg+1]
    (exp((gdpint$log.gdp[gdpint$year == end] - gdpint$log.gdp[gdpint$year == start])/(end-start))-1)*100
  }))
  
  gdpint$growth.ma15 <- filter(gdpint$growth, rep(1/15, 15), sides=2)
  gdpint$growth.ma19 <- filter(gdpint$growth, rep(1/19, 19), sides=2)
  
  plot <- ggplot(gdpint, aes(year, growth)) +
    coord_cartesian(ylim = c(-7, +7)) +
    scale_y_continuous(breaks = -7:7) +
    scale_x_continuous(breaks = 13:20*100) +
    theme_bw() +
    geom_line(colour = "gray90") +
    geom_point(colour = "gray90") + 
    geom_line(colour = "black", aes(year, growth.ma19))  
  
  for (reg in 1:length(regime.growth)) {
    x1 <- regimes[reg]
    x2 <- regimes[reg+1]
    y <- regimes.growth[reg]
    col <- regimes.colour[reg]
    print(paste(x1,x2,y,col))
    plot <- plot + geom_segment(x = x1, y = y, xend = x2, yend = y, colour=col)
  }
  
  return(plot)  
}
  
regimes <- c(1300,1700,1850,1870,1906,1928,1950,1972,1987,2007,2014)
regimes.colour <- c("blue","blue","blue","blue","red","red","red","red","red","red")  

gor.regimes.growth <- calc_regimes(gdp.gor, regimes, regimes.colour, TRUE)
   