library(tidyr)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
data <- read.csv('~/Desktop/nst-est2019-alldata.csv')
dt <- data.table(data)
njpop <-  dt[36,]
njpop2 <- njpop[,1:17]
njpop3 <-  njpop2[,-(1:7)]
njpop3 <- rename(njpop3, c('2010' = 'POPESTIMATE2010', '2011' = 'POPESTIMATE2011', '2012' = 'POPESTIMATE2012', '2013' = 'POPESTIMATE2013', '2014' = 'POPESTIMATE2014', '2015' = 'POPESTIMATE2015', '2016' = 'POPESTIMATE2016', '2017' = 'POPESTIMATE2017', '2018' = 'POPESTIMATE2018', '2019' = 'POPESTIMATE2019'))
njpop4 <- gather(njpop3, 'year', 'population')
njpop4$year <- as.integer(njpop4$year)
str(njpop4)
regression <- lm(population ~ year, njpop4)
regression
plot1 <- ggplot(njpop4) + geom_point(mapping = aes(x = year, y = population)) + scale_x_continuous(breaks = pretty_breaks()) + ggtitle('Population for New Jesrey') + theme(plot.title = element_text(hjust = .5)) + geom_abline(intercept = -8209120, slope = 8472)
plot1
summary(regression)
future <- data.frame(year = c(2025))
predict(regression, future)
