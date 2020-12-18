########################
## USGS Water Quality ##
########################

setwd("/Users/April/Desktop/Chapter 1/Code") #file path will need to be updated

# Load required packages
library(wql)
library(waterData)
library(ggplot2)
library(plotrix)
library(tibble)
library(ggpubr)

# Daily values- time series
# Temperature (mean)
East.TE <- importDVs("0208458892", code = "00010", stat = "00003", sdate = "2013-01-01",
          edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.TE <- importDVs("0208458893", code = "00010", stat = "00003", sdate = "2013-01-01",
                  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.TE <- rbind(East.TE, West.TE)
Lake.TE <- add_column(Lake.TE, var = "TE", .after = 1)

# Temperature (max)
East.TE.max <- importDVs("0208458892", code = "00010", stat = "00002", sdate = "2013-01-01",
                         edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.TE.max <- importDVs("0208458893", code = "00010", stat = "00002", sdate = "2013-01-01",
                         edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.TE.max <- rbind(East.TE.max, West.TE.max)
Lake.TE.max <- add_column(Lake.TE.max, var = "TE.max", .after = 1)

# DO (mean)
East.DO <- importDVs("0208458892", code = "00300", stat = "00003", sdate = "2013-01-01",
                  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.DO <- importDVs("0208458893", code = "00300", stat = "00003", sdate = "2013-01-01",
                  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.DO <- rbind(East.DO, West.DO)
Lake.DO <- add_column(Lake.DO, var = "DO", .after = 1)

# DO (min)
East.DO.min <- importDVs("0208458892", code = "00300", stat = "00002", sdate = "2013-01-01",
                         edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.DO.min <- importDVs("0208458893", code = "00300", stat = "00002", sdate = "2013-01-01",
                         edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.DO.min <- rbind(East.DO.min, West.DO.min)
Lake.DO.min <- add_column(Lake.DO.min, var = "DO.min", .after = 1)

# Salinity (mean)
East.S <- importDVs("0208458892", code = "00480", stat = "00003", sdate = "2013-01-01",
                  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.S <- importDVs("0208458893", code = "00480", stat = "00003", sdate = "2013-01-01",
                  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.S <- rbind(East.S, West.S)
Lake.S <- add_column(Lake.S, var = "S", .after = 1)

# pH (median)
East.PH <- importDVs("0208458892", code = "00400", stat = "00008", sdate = "2013-01-01",
                    edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.PH <- importDVs("0208458893", code = "00400", stat = "00008", sdate = "2013-01-01",
                    edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.PH <- rbind(East.PH, West.PH)
Lake.PH <- add_column(Lake.PH, var = "PH", .after = 1)

# Turbidity (mean)
East.TU <- importDVs("0208458892", code = "63680", stat = "00003", sdate = "2015-01-01",
                  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.TU <- importDVs("0208458893", code = "63680", stat = "00003", sdate = "2015-01-01",
                  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.TU <- rbind(East.TU, West.TU)
Lake.TU <- add_column(Lake.TU, var = "TU", .after = 1)

# Gauge Height (mean)
East.GH <- importDVs("0208458892", code = "00065", stat = "00003", sdate = "2013-01-01",
                  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.GH <- importDVs("0208458893", code = "00065", stat = "00003", sdate = "2013-01-01",
                  edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.GH <- rbind(East.GH, West.GH)
Lake.GH <- add_column(Lake.GH, var = "GH", .after = 1)

# Specific Conductance (mean)
East.SC <- importDVs("0208458892", code = "00095", stat = "00003", sdate = "2013-01-01",
                     edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.SC <- importDVs("0208458893", code = "00095", stat = "00003", sdate = "2013-01-01",
                     edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.SC <- rbind(East.SC, West.SC)
Lake.SC <- add_column(Lake.SC, var = "SC", .after = 1)

# Elevation (mean)
East.E <- importDVs("0208458892", code = "62615", stat = "00003", sdate = "2015-10-01",
                     edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
West.E <- importDVs("0208458893", code = "62615", stat = "00003", sdate = "2015-10-01",
                     edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Lake.E <- rbind(East.E, West.E)
Lake.E <- add_column(Lake.E, var = "E", .after = 1)

# Precipitation
Precip <- importDVs("352936076125245", code = "00045", stat = "00006", sdate = "2015-09-30",
                    edate = as.Date(Sys.Date(), format = "%Y-%m-%d"))
Precip <- cleanUp(Precip, task="fix", replace = 0)
Precip <- add_column(Precip, var = "P", .after = 1)

# Plot time series
# Temperature
# East
plot(East.TE$dates, East.TE$val, type="l",
     ylab="Daily median Water Temperature (C)",
     xlab="", yaxs='i', xaxs='i', ylim=c(-5, 40),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="red")
title("East of Hwy 94: Water Temperature",cex.main=0.95)
# West
plot(West.TE$dates, West.TE$val, type="l",
     ylab="Daily mean Water Temperature (C)",
     xlab="", yaxs='i', xaxs='i', ylim=c(-5, 40),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="blue")
title("West of Hwy 94: Water Temperature",cex.main=0.95)

# DO
# East
plot(East.DO$dates, East.DO$val, type="l",
     ylab="Daily mean Dissolved Oxygen (mg/L)",
     xlab="", yaxs='i', xaxs='i', ylim=c(4, 16),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="red")
title("East of Hwy 94: Dissolved Oxygen",cex.main=0.95)
# West
plot(West.DO$dates, West.DO$val, type="l",
     ylab="Daily mean Dissolved Oxygen (mg/L)",
     xlab="", yaxs='i', xaxs='i', ylim=c(4, 16),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="blue")
title("West of Hwy 94: Dissolved Oxygen",cex.main=0.95)

# Min DO
# East
plot(East.DO.min$dates, East.DO.min$val, type="l",
     ylab="Daily minimum Dissolved Oxygen (mg/L)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 16),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="red")
title("East of Hwy 94: Minimum Dissolved Oxygen",cex.main=0.95)
# West
plot(West.DO.min$dates, West.DO.min$val, type="l",
     ylab="Daily minimum Dissolved Oxygen (mg/L)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 16),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="blue")
title("West of Hwy 94: Dissolved Oxygen",cex.main=0.95)

# Salinity
# East
plot(East.S$dates, East.S$val, type="l",
     ylab="Daily mean Salinity, unfiltered (ppt)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 2),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="red")
title("East of Hwy 94: Salinity",cex.main=0.95)
# West
plot(West.S$dates, West.S$val, type="l",
     ylab="Daily mean Salinity, unfiltered (ppt)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 2),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="blue")
title("West of Hwy 94: Salinity",cex.main=0.95)

# pH
# East
plot(East.PH$dates, East.PH$val, type="l",
     ylab="Daily median pH",
     xlab="", yaxs='i', xaxs='i', ylim=c(6.5, 10.5),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="red")
title("East of Hwy 94: pH",cex.main=0.95)
# West
plot(West.PH$dates, West.PH$val, type="l",
     ylab="Daily median pH",
     xlab="", yaxs='i', xaxs='i', ylim=c(6.5, 10.5),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="blue")
title("West of Hwy 94: pH",cex.main=0.95)

# Turbidity
# East
plot(East.TU$dates, East.TU$val, type="l",
     ylab="Daily mean Turbidity (NEPH)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 400),
     xlim=c(as.Date("2015-01-01"), as.Date(Sys.Date())), col="red")
title("East of Hwy 94: Turbidity",cex.main=0.95)
# West
plot(West.TU$dates, West.TU$val, type="l",
     ylab="Daily mean Turbidity (NEPH)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 400),
     xlim=c(as.Date("2015-01-01"), as.Date(Sys.Date())), col="blue")
title("West of Hwy 94: Turbidity",cex.main=0.95)

# Gauge Height
# East
plot(East.GH$dates, East.GH$val, type="l",
     ylab="Daily mean Gauge Height (ft)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 4),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="red")
title("East of Hwy 94: Gauge Height",cex.main=0.95)
# West
plot(West.GH$dates, West.GH$val, type="l",
     ylab="Daily mean Gauge Height (ft)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 4),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="blue")
title("West of Hwy 94: Gauge Height",cex.main=0.95)

# Specific Conductance
# East
plot(East.SC$dates, East.SC$val, type="l",
     ylab="Daily mean Specific Conductance at 25C (uS/cm)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 3100),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="red")
title("East of Hwy 94: Specific Conductance",cex.main=0.95)
# West
plot(West.SC$dates, West.SC$val, type="l",
     ylab="Daily mean Specific Conductance at 25C (uS/cm)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 3100),
     xlim=c(as.Date("2013-01-01"), as.Date(Sys.Date())), col="blue")
title("West of Hwy 94: Specific Conductance",cex.main=0.95)

# Elevation
# East
plot(East.E$dates, East.E$val, type="l",
     ylab="Daily mean water surface elevation above NAVD 1988 (feet)",
     xlab="", yaxs='i', xaxs='i', ylim=c(-2, 2),
     xlim=c(as.Date("2015-10-01"), as.Date(Sys.Date())), col="red")
title("East of Hwy 94: Elevation",cex.main=0.95)
# West
plot(West.E$dates, West.E$val, type="l",
     ylab="Daily water surface elevation above NAVD 1988 (feet)",
     xlab="", yaxs='i', xaxs='i', ylim=c(-2, 2),
     xlim=c(as.Date("2015-10-01"), as.Date(Sys.Date())), col="blue")
title("West of Hwy 94: Elevation",cex.main=0.95)

# Precipitation
plot(Precip$dates, Precip$val, type="l",
     ylab="Daily precipitation (total inches)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 7),
     xlim=c(as.Date("2015-09-30"), as.Date(Sys.Date())), col="gray")
title("Precipitation",cex.main=0.95)


# Function to get summary stats
summary.list = function(x)list(
        N.with.NA.removed= length(x[!is.na(x)]),
        Count.of.NA= length(x[is.na(x)]),
        Mean=mean(x, na.rm=TRUE),
        Max.Min=range(x, na.rm=TRUE),
        Coeff.Variation.Prcnt=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100,
        Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))

# All summary stats of interest
# Temperature (mean)
summary.list(East.TE$val)
summary.list(West.TE$val)
summary.list(Lake.TE$val)

#Temperature (max)
summary.list(East.TE.max$val)
summary.list(West.TE.max$val)
summary.list(Lake.TE.max$val)

# DO (mean)
summary.list(East.DO$val)
summary.list(West.DO$val)
summary.list(Lake.DO$val)

# DO (min)
summary.list(East.DO.min$val)
summary.list(West.DO.min$val)
summary.list(Lake.DO.min$val)

# Salinity (mean)
summary.list(East.S$val)
summary.list(West.S$val)
summary.list(Lake.S$val)

# Turbidity (mean)
summary.list(East.TU$val)
summary.list(West.TU$val)
summary.list(Lake.TU$val)

# Gauge Height (mean)
summary.list(East.GH$val)
summary.list(West.GH$val)
summary.list(Lake.GH$val)

# Specific conductance (mean)
summary.list(East.SC$val)
summary.list(West.SC$val)
summaryStats(West.SC)
summaryStats(Lake.SC)

# Elevation (mean)
summary.list(East.E$val)
summary.list(West.E$val)
summary.list(Lake.E$val)

# pH (median)
summary.list(East.PH$val)
summary.list(West.PH$val)
summary.list(Lake.PH$val)

# Precipitation (total inches)
summary.list(Precip$val)

#############################
# Spearman Rank Correlation #
#############################

# Load required packages
library(wql)
library(waterData)
library(ggplot2)
library(plotrix)
library(tibble)

# Read in carp data
CC.pooled <- read.csv("/Users/April/Desktop/Chapter 1/Data/pooled_long.csv")

# Growth of fish by year
carp.2013 <- CC.pooled [ which(CC.pooled $Growth.Year=='2013'), ]
carp.2014 <- CC.pooled [ which(CC.pooled $Growth.Year=='2014'), ]
carp.2015 <- CC.pooled [ which(CC.pooled $Growth.Year=='2015'), ]
carp.2016 <- CC.pooled [ which(CC.pooled $Growth.Year=='2016'), ]
carp.2017 <- CC.pooled [ which(CC.pooled $Growth.Year=='2017'), ]

# Get avg growth by year
mean(carp.2013$Growth)
mean(carp.2014$Growth)
mean(carp.2015$Growth)
mean(carp.2016$Growth)
mean(carp.2017$Growth)

# Parse again by age
carp.2013.2 <- carp.2013 [ which(carp.2013 $BC.Age == '2'), ]
carp.2013.3 <- carp.2013 [ which(carp.2013 $BC.Age == '3'), ]
carp.2013.4 <- carp.2013 [ which(carp.2013 $BC.Age == '4'), ]
carp.2013.5 <- carp.2013 [ which(carp.2013 $BC.Age == '5'), ]
carp.2013.6 <- carp.2013 [ which(carp.2013 $BC.Age == '6'), ]

carp.2014.2 <- carp.2014 [ which(carp.2014$BC.Age == '2'), ]
carp.2014.3 <- carp.2014 [ which(carp.2014$BC.Age == '3'), ]
carp.2014.4 <- carp.2014 [ which(carp.2014$BC.Age == '4'), ]
carp.2014.5 <- carp.2014 [ which(carp.2014$BC.Age == '5'), ]
carp.2014.6 <- carp.2014 [ which(carp.2014$BC.Age == '6'), ]

carp.2015.2 <- carp.2015 [ which(carp.2015$BC.Age == '2'), ]
carp.2015.3 <- carp.2015 [ which(carp.2015$BC.Age == '3'), ]
carp.2015.4 <- carp.2015 [ which(carp.2015$BC.Age == '4'), ]
carp.2015.5 <- carp.2015 [ which(carp.2015$BC.Age == '5'), ]
carp.2015.6 <- carp.2015 [ which(carp.2015$BC.Age == '6'), ]

carp.2016.2<- carp.2016 [ which(carp.2016$BC.Age == '2'), ]
carp.2016.3<- carp.2016 [ which(carp.2016$BC.Age == '3'), ]
carp.2016.4<- carp.2016 [ which(carp.2016$BC.Age == '4'), ]
carp.2016.5<- carp.2016 [ which(carp.2016$BC.Age == '5'), ]
carp.2016.6<- carp.2016 [ which(carp.2016$BC.Age == '6'), ]

carp.2017.2<- carp.2017 [ which(carp.2017$BC.Age == '2'), ]
carp.2017.3<- carp.2017 [ which(carp.2017$BC.Age == '3'), ]
carp.2017.4<- carp.2017 [ which(carp.2017$BC.Age == '4'), ]
carp.2017.5<- carp.2017 [ which(carp.2017$BC.Age == '5'), ]
carp.2017.6<- carp.2017 [ which(carp.2017$BC.Age == '6'), ]

# Bind WQ variables
allWQ <- rbind(Lake.TE, Lake.TE.max, Lake.DO, Lake.DO.min, Lake.TU, Lake.E, Lake.GH, Lake.S, Lake.SC, Precip, Lake.PH)

# Now get mean by variable and year
# Temperature
TE.2013 <- allWQ[ which((allWQ$var=='TE') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
TE.2014 <- allWQ[ which((allWQ$var=='TE') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
TE.2015 <- allWQ[ which((allWQ$var=='TE') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
TE.2016 <- allWQ[ which((allWQ$var=='TE') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
TE.2017 <- allWQ[ which((allWQ$var=='TE') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(TE.2013$val)
summary.m(TE.2014$val)
summary.m(TE.2015$val)
summary.m(TE.2016$val)
summary.m(TE.2017$val)

# Max Temperature
TE.max.2013 <- allWQ[ which((allWQ$var=='TE.max') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
TE.max.2014 <- allWQ[ which((allWQ$var=='TE.max') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
TE.max.2015 <- allWQ[ which((allWQ$var=='TE.max') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
TE.max.2016 <- allWQ[ which((allWQ$var=='TE.max') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
TE.max.2017 <- allWQ[ which((allWQ$var=='TE.max') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(TE.max.2013$val)
summary.m(TE.max.2014$val)
summary.m(TE.max.2015$val)
summary.m(TE.max.2016$val)
summary.m(TE.max.2017$val)

# DO
DO.2013 <- allWQ[ which((allWQ$var=='DO') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
DO.2014 <- allWQ[ which((allWQ$var=='DO') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
DO.2017 <- allWQ[ which((allWQ$var=='DO') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
DO.2015 <- allWQ[ which((allWQ$var=='DO') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
DO.2016 <- allWQ[ which((allWQ$var=='DO') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(DO.2013$val)
summary.m(DO.2014$val)
summary.m(DO.2015$val)
summary.m(DO.2016$val)
summary.m(DO.2017$val)

# Min DO
DO.min.2013 <- allWQ[ which((allWQ$var=='DO.min') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
DO.min.2014 <- allWQ[ which((allWQ$var=='DO.min') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
DO.min.2015 <- allWQ[ which((allWQ$var=='DO.min') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
DO.min.2016 <- allWQ[ which((allWQ$var=='DO.min') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
DO.min.2017 <- allWQ[ which((allWQ$var=='DO.min') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(DO.min.2013$val)
summary.m(DO.min.2014$val)
summary.m(DO.min.2015$val)
summary.m(DO.min.2016$val)
summary.m(DO.min.2017$val)

# GH
GH.2013 <- allWQ[ which((allWQ$var=='GH') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
GH.2014 <- allWQ[ which((allWQ$var=='GH') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
GH.2015 <- allWQ[ which((allWQ$var=='GH') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
GH.2016 <- allWQ[ which((allWQ$var=='GH') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
GH.2017 <- allWQ[ which((allWQ$var=='GH') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(GH.2013$val)
summary.m(GH.2014$val)
summary.m(GH.2015$val)
summary.m(GH.2016$val)
summary.m(GH.2017$val)

# SC
SC.2013 <- allWQ[ which((allWQ$var=='SC') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
SC.2014 <- allWQ[ which((allWQ$var=='SC') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
SC.2015 <- allWQ[ which((allWQ$var=='SC') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
SC.2016 <- allWQ[ which((allWQ$var=='SC') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
SC.2017 <- allWQ[ which((allWQ$var=='SC') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(SC.2013$val)
summary.m(SC.2014$val)
summary.m(SC.2015$val)
summary.m(SC.2016$val)
summary.m(SC.2017$val)

# S
S.2013 <- allWQ[ which((allWQ$var=='S') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
S.2014 <- allWQ[ which((allWQ$var=='S') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
S.2015 <- allWQ[ which((allWQ$var=='S') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
S.2016 <- allWQ[ which((allWQ$var=='S') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
S.2017 <- allWQ[ which((allWQ$var=='S') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(S.2013$val)
summary.m(S.2014$val)
summary.m(S.2015$val)
summary.m(S.2016$val)
summary.m(S.2017$val)

# E
E.2013 <- allWQ[ which((allWQ$var=='E') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
E.2014 <- allWQ[ which((allWQ$var=='E') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
E.2015 <- allWQ[ which((allWQ$var=='E') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
E.2016 <- allWQ[ which((allWQ$var=='E') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
E.2017 <- allWQ[ which((allWQ$var=='E') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(E.2013$val)
summary.m(E.2014$val)
summary.m(E.2015$val)
summary.m(E.2016$val)
summary.m(E.2017$val)

# PH
PH.2013 <- allWQ[ which((allWQ$var=='PH') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
PH.2014 <- allWQ[ which((allWQ$var=='PH') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
PH.2015 <- allWQ[ which((allWQ$var=='PH') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
PH.2016 <- allWQ[ which((allWQ$var=='PH') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
PH.2017 <- allWQ[ which((allWQ$var=='PH') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(PH.2013$val)
summary.m(PH.2014$val)
summary.m(PH.2015$val)
summary.m(PH.2016$val)
summary.m(PH.2017$val)

# Precip
P.2013 <- allWQ[ which((allWQ$var=='P') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
P.2014 <- allWQ[ which((allWQ$var=='P') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
P.2015 <- allWQ[ which((allWQ$var=='P') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
P.2016 <- allWQ[ which((allWQ$var=='P') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
P.2017 <- allWQ[ which((allWQ$var=='P') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(P.2013$val)
summary.m(P.2014$val)
summary.m(P.2015$val)
summary.m(P.2016$val)
summary.m(P.2017$val)

# TU
TU.2013 <- allWQ[ which((allWQ$var=='TU') & (allWQ$dates>='2013-01-01') & (allWQ$dates<='2014-01-01')), ]
TU.2014 <- allWQ[ which((allWQ$var=='TU') & (allWQ$dates>='2014-01-01') & (allWQ$dates<='2015-01-01')), ]
TU.2017 <- allWQ[ which((allWQ$var=='TU') & (allWQ$dates>='2015-01-01') & (allWQ$dates<='2016-01-01')), ]
TU.2015 <- allWQ[ which((allWQ$var=='TU') & (allWQ$dates>='2016-01-01') & (allWQ$dates<='2017-01-01')), ]
TU.2016 <- allWQ[ which((allWQ$var=='TU') & (allWQ$dates>='2017-01-01') & (allWQ$dates<='2018-01-01')), ]

summary.m(TU.2013$val)
summary.m(TU.2014$val)
summary.m(TU.2015$val)
summary.m(TU.2016$val)
summary.m(TU.2017$val)

# Load in new table of carp growth + mbcl by age + environmental means
data <- read.csv("/Users/April/Desktop/Chapter 1/Data/growth_environment.csv")

# growth
ggscatter(data, x = "GH", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "Growth (mm)")
ggscatter(data, x = "TE", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "Growth (mm)")
ggscatter(data, x = "MaxTE", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "Growth (mm)")
ggscatter(data, x = "DO", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "Growth (mm)")
ggscatter(data, x = "MinDO", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "Growth (mm)")
ggscatter(data, x = "PH_median", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "Growth (mm)")
ggscatter(data, x = "TU", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "Growth (mm)")
ggscatter(data, x = "Precip", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "Growth (mm)")
ggscatter(data, x = "E", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "Growth (mm)")
ggscatter(data, x = "SC", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "Growth (mm)")

# MBCL-at-age 2
ggscatter(data, x = "GH", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "MBCL_age2")
ggscatter(data, x = "TE", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "MBCL_age2")
ggscatter(data, x = "MaxTE", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "MBCL_age2")
ggscatter(data, x = "DO", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "MBCL_age2")
ggscatter(data, x = "MinDO", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "MBCL_age2")
ggscatter(data, x = "PH_median", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "MBCL_age2")
ggscatter(data, x = "TU", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "MBCL_age2")
ggscatter(data, x = "Precip", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "MBCL_age2")
ggscatter(data, x = "E", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "MBCL_age2")
ggscatter(data, x = "SC", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "MBCL_age2")
ggscatter(data, x = "S", y = "MBCL_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "S", ylab = "MBCL_age2")

# MBCL-at-age 3
ggscatter(data, x = "GH", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "MBCL_age3")
ggscatter(data, x = "TE", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "MBCL_age3")
ggscatter(data, x = "MaxTE", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "MBCL_age3")
ggscatter(data, x = "DO", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "MBCL_age3")
ggscatter(data, x = "MinDO", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "MBCL_age3")
ggscatter(data, x = "PH_median", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "MBCL_age3")
ggscatter(data, x = "TU", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "MBCL_age3")
ggscatter(data, x = "Precip", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "MBCL_age3")
ggscatter(data, x = "E", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "MBCL_age3")
ggscatter(data, x = "SC", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "MBCL_age3")
ggscatter(data, x = "S", y = "MBCL_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "S", ylab = "MBCL_age3")

# MBCL-at-age 4
ggscatter(data, x = "GH", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "MBCL_age4")
ggscatter(data, x = "TE", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "MBCL_age4")
ggscatter(data, x = "MaxTE", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "MBCL_age4")
ggscatter(data, x = "DO", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "MBCL_age4")
ggscatter(data, x = "MinDO", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "MBCL_age4")
ggscatter(data, x = "PH_median", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "MBCL_age4")
ggscatter(data, x = "TU", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "MBCL_age4")
ggscatter(data, x = "Precip", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "MBCL_age4")
ggscatter(data, x = "E", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "MBCL_age4")
ggscatter(data, x = "SC", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "MBCL_age4")
ggscatter(data, x = "S", y = "MBCL_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "S", ylab = "MBCL_age4")

# MBCL-at-age 5
ggscatter(data, x = "GH", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "MBCL_age5")
ggscatter(data, x = "TE", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "MBCL_age5")
ggscatter(data, x = "MaxTE", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "MBCL_age5")
ggscatter(data, x = "DO", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "MBCL_age5")
ggscatter(data, x = "MinDO", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "MBCL_age5")
ggscatter(data, x = "PH_median", y = "Carp.growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "MBCL_age5")
ggscatter(data, x = "TU", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "MBCL_age5")
ggscatter(data, x = "Precip", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "MBCL_age5")
ggscatter(data, x = "E", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "MBCL_age5")
ggscatter(data, x = "SC", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "MBCL_age5")
ggscatter(data, x = "S", y = "MBCL_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "S", ylab = "MBCL_age5")

# growth for age 2 fish
ggscatter(data, x = "GH", y = "growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "growth_age2")
ggscatter(data, x = "TE", y = "growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "growth_age2")
ggscatter(data, x = "MaxTE", y = "growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "growth_age2")
ggscatter(data, x = "DO", y = "growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "growth_age2")
ggscatter(data, x = "MinDO", y = "growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "growth_age2")
ggscatter(data, x = "PH_median", y = "growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "growth_age2")
ggscatter(data, x = "TU", y = "growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "growth_age2")
ggscatter(data, x = "Precip", y = "growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "growth_age2")
ggscatter(data, x = "E", y = "growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "growth_age2")
ggscatter(data, x = "SC", y = "Carp.growth_age2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "growth_age2")


# growth for age 3 fish
ggscatter(data, x = "GH", y = "growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "growth_age3")
ggscatter(data, x = "TE", y = "growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "growth_age3")
ggscatter(data, x = "MaxTE", y = "growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "growth_age3")
ggscatter(data, x = "DO", y = "growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "growth_age3")
ggscatter(data, x = "MinDO", y = "growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "growth_age3")
ggscatter(data, x = "PH_median", y = "growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "growth_age3")
ggscatter(data, x = "TU", y = "growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "growth_age3")
ggscatter(data, x = "Precip", y = "growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "growth_age3")
ggscatter(data, x = "E", y = "growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "growth_age3")
ggscatter(data, x = "SC", y = "Carp.growth_age3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "growth_age3")


# growth for age 4 fish
ggscatter(data, x = "GH", y = "growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "growth_age4")
ggscatter(data, x = "TE", y = "growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "growth_age4")
ggscatter(data, x = "MaxTE", y = "growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "growth_age4")
ggscatter(data, x = "DO", y = "growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "growth_age4")
ggscatter(data, x = "MinDO", y = "growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "growth_age4")
ggscatter(data, x = "PH_median", y = "growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "growth_age4")
ggscatter(data, x = "TU", y = "growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "growth_age4")
ggscatter(data, x = "Precip", y = "growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "growth_age4")
ggscatter(data, x = "E", y = "growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "growth_age4")
ggscatter(data, x = "SC", y = "Carp.growth_age4", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "growth_age4")

# growth for age 5 fish
ggscatter(data, x = "GH", y = "growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "growth_age5")
ggscatter(data, x = "TE", y = "growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "growth_age5")
ggscatter(data, x = "MaxTE", y = "growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "growth_age5")
ggscatter(data, x = "DO", y = "growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "growth_age5")
ggscatter(data, x = "MinDO", y = "growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "growth_age5")
ggscatter(data, x = "PH_median", y = "growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "growth_age5")
ggscatter(data, x = "TU", y = "growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "growth_age5")
ggscatter(data, x = "Precip", y = "growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "growth_age5")
ggscatter(data, x = "E", y = "growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "growth_age5")
ggscatter(data, x = "SC", y = "Carp.growth_age5", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "growth_age5")

# growth for age 6 fish
ggscatter(data, x = "GH", y = "growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gauge height", ylab = "growth_age6")
ggscatter(data, x = "TE", y = "growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp", ylab = "growth_age6")
ggscatter(data, x = "MaxTE", y = "growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Temp Max", ylab = "growth_age6")
ggscatter(data, x = "DO", y = "growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "DO", ylab = "growth_age6")
ggscatter(data, x = "MinDO", y = "growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Min DO", ylab = "growth_age6")
ggscatter(data, x = "PH_median", y = "growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PH", ylab = "growth_age6")
ggscatter(data, x = "TU", y = "growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "TU", ylab = "growth_age6")
ggscatter(data, x = "Precip", y = "growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Precip", ylab = "growth_age6")
ggscatter(data, x = "E", y = "growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "E", ylab = "growth_age6")
ggscatter(data, x = "SC", y = "Carp.growth_age6", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "SC", ylab = "growth_age6")
