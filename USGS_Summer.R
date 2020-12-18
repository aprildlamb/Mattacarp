#########################################
## USGS Water Quality- Summer Extremes ##
#########################################

setwd("/Users/April/Desktop/Chapter 1/Code") #file path will need to be updated

# Load required packages
library(wql)
library(waterData)
library(ggplot2)
library(plotrix)

# Daily values by year- summer extremes
# 2013
# Temperature (mean)
East.TE.s13 <- importDVs("0208458892", code = "00010", stat = "00003", sdate = "2013-06-21",
                     edate = "2013-09-22")
West.TE.s13 <- importDVs("0208458893", code = "00010", stat = "00003", sdate = "2013-06-21",
                     edate = "2013-09-22")
Lake.TE.s13 <- rbind(East.TE.s13, West.TE.s13)

# Temperature (max)
East.TE.max.s13 <- importDVs("0208458892", code = "00010", stat = "00001", sdate = "2013-06-21",
                         edate = "2013-09-22")
West.TE.max.s13 <- importDVs("0208458893", code = "00010", stat = "00001", sdate = "2013-06-21",
                         edate = "2013-09-22")
Lake.TE.max.s13 <- rbind(East.TE.max.s13, West.TE.max.s13)

# DO (mean)
East.DO.s13 <- importDVs("0208458892", code = "00300", stat = "00003", sdate = "2013-06-21",
                     edate = "2013-09-22")
West.DO.s13 <- importDVs("0208458893", code = "00300", stat = "00003", sdate = "2013-06-21",
                     edate = "2013-09-22")
Lake.DO.s13 <- rbind(East.DO.s13, West.DO.s13)

# DO (min)
East.DO.min.s13 <- importDVs("0208458892", code = "00300", stat = "00002", sdate = "2013-06-21",
                         edate = "2013-09-22")
West.DO.min.s13 <- importDVs("0208458893", code = "00300", stat = "00002", sdate = "2013-06-21",
                         edate = "2013-09-22")
Lake.DO.min.s13 <- rbind(East.DO.min.s13, West.DO.min.s13)

# 2014
# Temperature (mean)
East.TE.s14 <- importDVs("0208458892", code = "00010", stat = "00003", sdate = "2014-06-21",
                         edate = "2014-09-22")
West.TE.s14 <- importDVs("0208458893", code = "00010", stat = "00003", sdate = "2014-06-21",
                         edate = "2014-09-22")
Lake.TE.s14 <- rbind(East.TE.s14, West.TE.s14)

# Temperature (max)
East.TE.max.s14 <- importDVs("0208458892", code = "00010", stat = "00001", sdate = "2014-06-21",
                             edate = "2014-09-22")
West.TE.max.s14 <- importDVs("0208458893", code = "00010", stat = "00001", sdate = "2014-06-21",
                             edate = "2014-09-22")
Lake.TE.max.s14 <- rbind(East.TE.max.s14, West.TE.max.s14)

# DO (mean)
East.DO.s14 <- importDVs("0208458892", code = "00300", stat = "00003", sdate = "2014-06-21",
                         edate = "2014-09-22")
West.DO.s14 <- importDVs("0208458893", code = "00300", stat = "00003", sdate = "2014-06-21",
                         edate = "2014-09-22")
Lake.DO.s14 <- rbind(East.DO.s14, West.DO.s14)

# DO (min)
East.DO.min.s14 <- importDVs("0208458892", code = "00300", stat = "00002", sdate = "2014-06-21",
                             edate = "2014-09-22")
West.DO.min.s14 <- importDVs("0208458893", code = "00300", stat = "00002", sdate = "2014-06-21",
                             edate = "2014-09-22")
Lake.DO.min.s14 <- rbind(East.DO.min.s14, West.DO.min.s14)

# 2015
# Temperature (mean)
East.TE.s15 <- importDVs("0208458892", code = "00010", stat = "00003", sdate = "2015-06-21",
                         edate = "2015-09-22")
West.TE.s15 <- importDVs("0208458893", code = "00010", stat = "00003", sdate = "2015-06-21",
                         edate = "2015-09-22")
Lake.TE.s15 <- rbind(East.TE.s15, West.TE.s15)

# Temperature (max)
East.TE.max.s15 <- importDVs("0208458892", code = "00010", stat = "00001", sdate = "2015-06-21",
                             edate = "2015-09-22")
West.TE.max.s15 <- importDVs("0208458893", code = "00010", stat = "00001", sdate = "2015-06-21",
                             edate = "2015-09-22")
Lake.TE.max.s15 <- rbind(East.TE.max.s15, West.TE.max.s15)

# DO (mean)
East.DO.s15 <- importDVs("0208458892", code = "00300", stat = "00003", sdate = "2015-06-21",
                         edate = "2015-09-22")
West.DO.s15 <- importDVs("0208458893", code = "00300", stat = "00003", sdate = "2015-06-21",
                         edate = "2015-09-22")
Lake.DO.s15 <- rbind(East.DO.s15, West.DO.s15)

# DO (min)
East.DO.min.s15 <- importDVs("0208458892", code = "00300", stat = "00002", sdate = "2015-06-21",
                             edate = "2015-09-22")
West.DO.min.s15 <- importDVs("0208458893", code = "00300", stat = "00002", sdate = "2015-06-21",
                             edate = "2015-09-22")
Lake.DO.min.s15 <- rbind(East.DO.min.s15, West.DO.min.s15)


# 2016
# Temperature (mean)
East.TE.s16 <- importDVs("0208458892", code = "00010", stat = "00003", sdate = "2016-06-21",
                         edate = "2016-09-22")
West.TE.s16 <- importDVs("0208458893", code = "00010", stat = "00003", sdate = "2016-06-21",
                         edate = "2016-09-22")
Lake.TE.s16 <- rbind(East.TE.s16, West.TE.s16)

# Temperature (max)
East.TE.max.s16 <- importDVs("0208458892", code = "00010", stat = "00001", sdate = "2016-06-21",
                             edate = "2016-09-22")
West.TE.max.s16 <- importDVs("0208458893", code = "00010", stat = "00001", sdate = "2016-06-21",
                             edate = "2016-09-22")
Lake.TE.max.s16 <- rbind(East.TE.max.s16, West.TE.max.s16)

# DO (mean)
East.DO.s16 <- importDVs("0208458892", code = "00300", stat = "00003", sdate = "2016-06-21",
                         edate = "2016-09-22")
West.DO.s16 <- importDVs("0208458893", code = "00300", stat = "00003", sdate = "2016-06-21",
                         edate = "2016-09-22")
Lake.DO.s16 <- rbind(East.DO.s16, West.DO.s16)

# DO (min)
East.DO.min.s16 <- importDVs("0208458892", code = "00300", stat = "00002", sdate = "2016-06-21",
                             edate = "2016-09-22")
West.DO.min.s16 <- importDVs("0208458893", code = "00300", stat = "00002", sdate = "2016-06-21",
                             edate = "2016-09-22")
Lake.DO.min.s16 <- rbind(East.DO.min.s16, West.DO.min.s16)

# 2017
# Temperature (mean)
East.TE.s17 <- importDVs("0208458892", code = "00010", stat = "00003", sdate = "2017-06-21",
                         edate = "2017-09-22")
West.TE.s17 <- importDVs("0208458893", code = "00010", stat = "00003", sdate = "2017-06-21",
                         edate = "2017-09-22")
Lake.TE.s17 <- rbind(East.TE.s17, West.TE.s17)

# Temperature (max)
East.TE.max.s17 <- importDVs("0208458892", code = "00010", stat = "00001", sdate = "2017-06-21",
                             edate = "2017-09-22")
West.TE.max.s17 <- importDVs("0208458893", code = "00010", stat = "00001", sdate = "2017-06-21",
                             edate = "2017-09-22")
Lake.TE.max.s17 <- rbind(East.TE.max.s17, West.TE.max.s17)

# DO (mean)
East.DO.s17 <- importDVs("0208458892", code = "00300", stat = "00003", sdate = "2017-06-21",
                         edate = "2017-09-22")
West.DO.s17 <- importDVs("0208458893", code = "00300", stat = "00003", sdate = "2017-06-21",
                         edate = "2017-09-22")
Lake.DO.s17 <- rbind(East.DO.s17, West.DO.s17)

# DO (min)
East.DO.min.s17 <- importDVs("0208458892", code = "00300", stat = "00002", sdate = "2017-06-21",
                             edate = "2017-09-22")
West.DO.min.s17 <- importDVs("0208458893", code = "00300", stat = "00002", sdate = "2017-06-21",
                             edate = "2017-09-22")
Lake.DO.min.s17 <- rbind(East.DO.min.s17, West.DO.min.s17)


# 2018
# Temperature (mean)
East.TE.s18 <- importDVs("0208458892", code = "00010", stat = "00003", sdate = "2018-06-21",
                         edate = "2018-09-22")
West.TE.s18 <- importDVs("0208458893", code = "00010", stat = "00003", sdate = "2018-06-21",
                         edate = "2018-09-22")
Lake.TE.s18 <- rbind(East.TE.s18, West.TE.s18)

# Temperature (max)
East.TE.max.s18 <- importDVs("0208458892", code = "00010", stat = "00001", sdate = "2018-06-21",
                             edate = "2018-09-22")
West.TE.max.s18 <- importDVs("0208458893", code = "00010", stat = "00001", sdate = "2018-06-21",
                             edate = "2018-09-22")
Lake.TE.max.s18 <- rbind(East.TE.max.s18, West.TE.max.s18)

# DO (mean)
East.DO.s18 <- importDVs("0208458892", code = "00300", stat = "00003", sdate = "2018-06-21",
                         edate = "2018-09-22")
West.DO.s18 <- importDVs("0208458893", code = "00300", stat = "00003", sdate = "2018-06-21",
                         edate = "2018-09-22")
Lake.DO.s18 <- rbind(East.DO.s18, West.DO.s18)

# DO (min)
East.DO.min.s18 <- importDVs("0208458892", code = "00300", stat = "00002", sdate = "2018-06-21",
                             edate = "2018-09-22")
West.DO.min.s18 <- importDVs("0208458893", code = "00300", stat = "00002", sdate = "2018-06-21",
                             edate = "2018-09-22")
Lake.DO.min.s18 <- rbind(East.DO.min.s18, West.DO.min.s18)


# 2019
# Temperature (mean)
East.TE.s19 <- importDVs("0208458892", code = "00010", stat = "00003", sdate = "2019-06-21",
                         edate = "2019-09-22")
West.TE.s19 <- importDVs("0208458893", code = "00010", stat = "00003", sdate = "2019-06-21",
                         edate = "2019-09-22")
Lake.TE.s19 <- rbind(East.TE.s19, West.TE.s19)

# Temperature (max)
East.TE.max.s19 <- importDVs("0208458892", code = "00010", stat = "00001", sdate = "2019-06-21",
                           edate = "2019-09-22")
West.TE.max.s19 <- importDVs("0208458893", code = "00010", stat = "00001", sdate = "2019-06-21",
                           edate = "2019-09-22")
Lake.TE.max.s19 <- rbind(East.TE.max.s19, West.TE.max.s19)

# DO (mean)
East.DO.s19 <- importDVs("0208458892", code = "00300", stat = "00003", sdate = "2019-06-21",
                         edate = "2019-09-22")
West.DO.s19 <- importDVs("0208458893", code = "00300", stat = "00003", sdate = "2019-06-21",
                         edate = "2019-09-22")
Lake.DO.s19 <- rbind(East.DO.s19, West.DO.s19)

# DO (min)
East.DO.min.s19 <- importDVs("0208458892", code = "00300", stat = "00002", sdate = "2019-06-21",
                           edate = "2019-09-22")
West.DO.min.s19 <- importDVs("0208458893", code = "00300", stat = "00002", sdate = "2019-06-21",
                           edate = "2019-09-22")
Lake.DO.min.s19 <- rbind(East.DO.min.s19, West.DO.min.s19)

# Function to get summary stats
summary.list = function(x)list(
  N.with.NA.removed= length(x[!is.na(x)]),
  Count.of.NA= length(x[is.na(x)]),
  Mean=mean(x, na.rm=TRUE),
  Max.Min=range(x, na.rm=TRUE),
  Coeff.Variation.Prcnt=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100,
  Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))

# Summary stats by year
# 2013
# Temperature (mean)
summary.list(East.TE.s13$val)
summaryStats(East.TE.s13)
summary.list(West.TE.s13$val)
summaryStats(West.TE.s13)
summaryStats(Lake.TE.s13)

# Temperature (max)
summary.list(East.TE.max.s13$val)
summaryStats(East.TE.max.s13)
summary.list(West.TE.max.s13$val)
summaryStats(West.TE.max.s13)
summaryStats(Lake.TE.max.s13)

# DO (mean)
summary.list(East.DO.s13$val)
summaryStats(East.DO.s13)
summary.list(West.DO.s13$val)
summaryStats(West.DO.s13)
summaryStats(Lake.DO.s13)

# DO (min)
summary.list(East.DO.min.s13$val)
summaryStats(East.DO.min.s13)
summary.list(West.DO.min.s13$val)
summaryStats(West.DO.min.s13)
summaryStats(Lake.DO.min.s13)

# 2014
# Temperature (mean)
summary.list(East.TE.s14$val)
summaryStats(East.TE.s14)
summary.list(West.TE.s14$val)
summaryStats(West.TE.s14)
TE.SS <- summary.list(Lake.TE.s14$val)
summaryStats(Lake.TE.s14)

# Temperature (max)
summary.list(East.TE.max.s14$val)
summaryStats(East.TE.max.s14)
summary.list(West.TE.max.s14$val)
summaryStats(West.TE.max.s14)
TE.maxsummary.list(Lake.TE.max.s14$val)
summaryStats(Lake.TE.max.s14)

# DO (mean)
summary.list(East.DO.s14$val)
summaryStats(East.DO.s14)
summary.list(West.DO.s14$val)
summaryStats(West.DO.s14)
summary.list(Lake.DO.s14$val)
summaryStats(Lake.DO.s14)

# DO (min)

summary.list(East.DO.min.s14$val)
summaryStats(East.DO.min.s14)
summary.list(West.DO.min.s14$val)
summaryStats(West.DO.min.s14)
summary.list(Lake.DO.min.s14$val)
summaryStats(Lake.DO.min.s14)

# 2015
# Temperature (mean)
summary.list(East.TE.s15$val)
summaryStats(East.TE.s15)
summary.list(West.TE.s15$val)
summaryStats(West.TE.s15)
summary.list(Lake.TE.s15$val)
summaryStats(Lake.TE.s15)

# Temperature (max)
summary.list(East.TE.max.s15$val)
summaryStats(East.TE.max.s15)
summary.list(West.TE.max.s15$val)
summaryStats(West.TE.max.s15)
summary.list(Lake.TE.max.s15$val)
summaryStats(Lake.TE.max.s15)

# DO (mean)
summary.list(East.DO.s15$val)
summaryStats(East.DO.s15)
summary.list(West.DO.s15$val)
summaryStats(West.DO.s15)
summary.list(Lake.DO.s15$val)
summaryStats(Lake.DO.s15)

# DO (min)
summary.list(East.DO.min.s15$val)
summaryStats(East.DO.min.s15)
summary.list(West.DO.min.s15$val)
summaryStats(West.DO.min.s15)
summary.list(Lake.DO.min.s15$val)
summaryStats(Lake.DO.min.s15)

# 2016
# Temperature (mean)
summary.list(East.TE.s16$val)
summaryStats(East.TE.s16)
summary.list(West.TE.s16$val)
summaryStats(West.TE.s16)
summary.list(Lake.TE.s16$val)
summaryStats(Lake.TE.s16)

# Temperature (max)
summary.list(East.TE.max.s16$val)
summaryStats(East.TE.max.s16)
summary.list(West.TE.max.s16$val)
summaryStats(West.TE.max.s16)
summary.list(Lake.TE.max.s16$val)
summaryStats(Lake.TE.max.s16)

# DO (mean)
summary.list(East.DO.s16$val)
summaryStats(East.DO.s16)
summary.list(West.DO.s16$val)
summaryStats(West.DO.s16)
summary.list(Lake.DO.s16$val)
summaryStats(Lake.DO.s16)

# DO (min)
summary.list(East.DO.min.s16$val)
summaryStats(East.DO.min.s16)
summary.list(West.DO.min.s16$val)
summaryStats(West.DO.min.s16)
summary.list(Lake.DO.min.s16$val)
summaryStats(Lake.DO.min.s16)

# 2017
# Temperature (mean)
summary.list(East.TE.s17$val)
summaryStats(East.TE.s17)
summary.list(West.TE.s17$val)
summaryStats(West.TE.s17)
summary.list(Lake.TE.s17$val)
summaryStats(Lake.TE.s17)

# Temperature (max)
summary.list(East.TE.max.s17$val)
summaryStats(East.TE.max.s17)
summary.list(West.TE.max.s17$val)
summaryStats(West.TE.max.s17)
summary.list(Lake.TE.max.s17$val)
summaryStats(Lake.TE.max.s17)

# DO (mean)
summary.list(East.DO.s17$val)
summaryStats(East.DO.s17)
summary.list(West.DO.s17$val)
summaryStats(West.DO.s17)
summary.list(Lake.DO.s17$val)
summaryStats(Lake.DO.s17)

# DO (min)
summary.list(East.DO.min.s17$val)
summaryStats(East.DO.min.s17)
summary.list(West.DO.min.s17$val)
summaryStats(West.DO.min.s17)
summary.list(Lake.DO.min.s17$val)
summaryStats(Lake.DO.min.s17)

# 2018
# Temperature (mean)
summary.list(East.TE.s18$val)
summaryStats(East.TE.s18)
summary.list(West.TE.s18$val)
summaryStats(West.TE.s18)
summary.list(Lake.TE.s18$val)
summaryStats(Lake.TE.s18)

# Temperature (max)
summary.list(East.TE.max.s18$val)
summaryStats(East.TE.max.s18)
summary.list(West.TE.max.s18$val)
summaryStats(West.TE.max.s18)
summary.list(Lake.TE.max.s18$val)
summaryStats(Lake.TE.max.s18)

# DO (mean)
summary.list(East.DO.s18$val)
summaryStats(East.DO.s18)
summary.list(West.DO.s18$val)
summaryStats(West.DO.s18)
summary.list(Lake.DO.s18$val)
summaryStats(Lake.DO.s18)

# DO (min)
summary.list(East.DO.min.s18$val)
summaryStats(East.DO.min.s18)
summary.list(West.DO.min.s18$val)
summaryStats(West.DO.min.s18)
summary.list(Lake.DO.min.s18$val)
summaryStats(Lake.DO.min.s18)

# 2019
# Temperature (mean)
summary.list(East.TE.s19$val)
summaryStats(East.TE.s19)
summary.list(West.TE.s19$val)
summaryStats(West.TE.s19)
summary.list(Lake.TE.s19$val)
summaryStats(Lake.TE.s19)

# Temperature (max)
summary.list(East.TE.max.s19$val)
summaryStats(East.TE.max.s19)
summary.list(West.TE.max.s19$val)
summaryStats(West.TE.max.s19)
summary.list(Lake.TE.max.s19$val)
summaryStats(Lake.TE.max.s19)

# DO (mean)
summary.list(East.DO.s19$val)
summaryStats(East.DO.s19)
summary.list(West.DO.s19$val)
summaryStats(West.DO.s19)
summary.list(Lake.DO.s19$val)
summaryStats(Lake.DO.s19)

# DO (min)
summary.list(East.DO.min.s19$val)
summaryStats(East.DO.min.s19)
summary.list(West.DO.min.s19$val)
summaryStats(West.DO.min.s19)
summary.list(Lake.DO.min.s19$val)
summaryStats(Lake.DO.min.s19)




