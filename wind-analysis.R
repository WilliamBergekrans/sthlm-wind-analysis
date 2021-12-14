library(readr)
library(hms)
library(ggplot2)
library(dplyr)

#################################################################
# Import the data
#################################################################
landsort <- read_delim("wind-data/landsort.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

berga <- read_delim("wind-data/berga.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

skarpo <- read_delim("wind-data/skarpo.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

svenska_hogarna <- read_delim("wind-data/svenska-hogarna.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

svenska_hogarna_exakt <- read_delim("wind-data/svenska-hogarna-exaktare.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

soderarm <- read_delim("wind-data/soderarm.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

#################################################################
# Keep only observations made after 1995
#################################################################
landsort <- subset(landsort, datum >= as.Date("1996-01-01"))
berga <- subset(berga, datum >= as.Date("1996-01-01"))
skarpo <- subset(skarpo, datum >= as.Date("1996-01-01"))
svenska_hogarna <- subset(svenska_hogarna, datum >= as.Date("1996-01-01"))
soderarm <- subset(soderarm, datum >= as.Date("1996-01-01"))

# Merge the two svenska hogarna data sets
svenska_hogarna <- subset(svenska_hogarna, datum <= as.Date("2010-05-31"))
svenska_hogarna <- rbind(svenska_hogarna, svenska_hogarna_exakt)
rm(svenska_hogarna_exakt)

# Keep the same last date
landsort <- subset(landsort, datum <= as.Date("2021-06-30"))
berga <- subset(berga, datum <= as.Date("2021-06-30"))
skarpo <- subset(skarpo, datum <= as.Date("2021-06-30"))
svenska_hogarna <- subset(svenska_hogarna, datum <= as.Date("2021-06-30"))
soderarm <- subset(soderarm, datum <= as.Date("2021-06-30"))

#################################################################
# Remove some features
#################################################################
landsort <- subset(landsort, select = -c(kvalitet_1, kvalitet_2))
berga <- subset(berga, select = -c(kvalitet_1, kvalitet_2))
skarpo <- subset(skarpo, select = -c(kvalitet_1, kvalitet_2))
svenska_hogarna <- subset(svenska_hogarna, select = -c(kvalitet_1, kvalitet_2))
soderarm <- subset(soderarm, select = -c(kvalitet_1, kvalitet_2))


#################################################################
# Keep all observations made every third hour. 
# 8 observations per day
#################################################################
tider <- c(as_hms("00:00:00"), as_hms("03:00:00"), as_hms("06:00:00"), as_hms("09:00:00"), as_hms("12:00:00"), as_hms("15:00:00"), as_hms("18:00:00"), as_hms("21:00:00"))

landsort <- subset(landsort, tid %in% tider)
berga <- subset(berga, tid %in% tider)
skarpo <- subset(skarpo, tid %in% tider)
svenska_hogarna <- subset(svenska_hogarna, tid %in% tider)
soderarm <- subset(soderarm, tid %in% tider)

#################################################################
# New variable for the month (without year)
#################################################################
landsort$monad <- format(as.Date(landsort$datum,format="%Y-%m-%d"), format = "%m-%d")
berga$monad <- format(as.Date(berga$datum,format="%Y-%m-%d"), format = "%m-%d")
skarpo$monad <- format(as.Date(skarpo$datum,format="%Y-%m-%d"), format = "%m-%d")
svenska_hogarna$monad <- format(as.Date(svenska_hogarna$datum,format="%Y-%m-%d"), format = "%m-%d")
soderarm$monad <- format(as.Date(soderarm$datum,format="%Y-%m-%d"), format = "%m-%d")

## General Wind speed analysis
Intuitively wind speed should follow a normal distribution. The following plot show the distribution of different wind speeds at Landsort during 2020.

landsort2020 <- subset(landsort, format(datum, "%Y") == "2020" )

ggplot(landsort2020, aes(x = vindhastighet)) + 
  geom_density(color = "steelblue", size = 1, fill = "lightblue", alpha = 0.5) + 
  ggtitle("Vindhastighet densitetskurva") + 
  ylab("densitet")

# Moving average function 
# Filter from stats::filter, only needed if dplyr is in the project
ma <- function(x, n = 30){stats::filter(x, rep(1 / n, n), sides = 2)}

landsort2019 <- subset(landsort, format(datum, "%Y") == "2019" )
landsort2018 <- subset(landsort, format(datum, "%Y") == "2018" )
landsort2017 <- subset(landsort, format(datum, "%Y") == "2017" )

res <- merge(landsort2017, landsort2018, by.x = c("monad", "tid"), by.y = c("monad", "tid"), suffixes = c("17", "18"))
res2 <- merge(res, landsort2019, by.x = c("monad", "tid"), by.y = c("monad", "tid"), suffixes = c("1718", "19"))
res3 <- merge(res2, landsort2020, by.x = c("monad", "tid"), by.y = c("monad", "tid"), suffixes = c("19", "20"))

ggplot(res3, aes(x = monad)) + 
  geom_line(aes(y=ma(vindhastighet20), color = "2020")) +
  geom_line(aes(y=ma(vindhastighet19), color = "2019")) +
  geom_line(aes(y=ma(vindhastighet18), color = "2018")) +
  geom_line(aes(y=ma(vindhastighet17), color = "2017")) +
  ggtitle("Wind strength - Moving average - Landsort") + 
  ylab("Wind m/s") + 
  xlab(NULL)

# Fit a line, probably a GP. 
