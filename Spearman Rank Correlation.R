 
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
CC.2016 <-read.csv("/Users/April/Desktop/Chapter 1/Data/2016_long.csv")
CC.2017 <-read.csv("/Users/April/Desktop/Chapter 1/Data/2017_long.csv")
CC.2018 <-read.csv("/Users/April/Desktop/Chapter 1/Data/2018_long.csv")
CC.pooled <- read.csv("/Users/April/Desktop/Chapter 1/Data/pooled_long.csv")

# Growth of fish by year
yall <- subset(CC.pooled, Growth.Year>=2013)
y2 <- yall[ which(yall$BC.Age=='2'), ]
y3 <- yall[ which(yall$BC.Age=='3'), ]
y4 <- yall[ which(yall$BC.Age=='4'), ]
y5 <- yall[ which(yall$BC.Age=='5'), ]
y6 <- yall[ which(yall$BC.Age=='6'), ]

# Daily values
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

# All variables
allWQ <- rbind (Lake.TE, Lake.TE.max, Lake.DO, Lake.DO.min, Lake.TU, Lake.E, Lake.GH, Lake.S, Lake.SC, Precip, Lake.PH)


# Envrionmental Characteristics by year
y2013 <- subset(environ, Year = 2013)
y2014 <- subset(environ, Year = 2014)
y2015 <- subset(environ, Year = 2015)
y2016 <- subset(environ, Year = 2016)
y2017 <- subset(environ, Year = 2017)

