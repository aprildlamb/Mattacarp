############################################
## Common Carp Population Characteristics ##
############################################

setwd("/Users/April/Desktop/Chapter 1/Code") #file path will need to be updated

# Load required packages
library(FSA)      # for filterD(), headtail()
library(dplyr)    # for %>%, select(), arrange(), unique(), group_by, et al.
library(magrittr) # for %<>%
library(ggplot2)  # for all of the plotting functions
library(nlme)     # for nlme()
library(viridis)  # for vivid colors in ggplot2
library(RColorBrewer)
library(reshape2)
library(plotrix)
library(Matching)
library(multcomp)
library(car)
library(nlstools)

#######################################
## Length, Weight, and Age Frequency ##
#######################################

# Read in data
CCrw <- read.csv("/Users/April/Desktop/Chapter 1/Data/carp_condition.csv")
headtail(CCrw,n=2)

# Append appropriate intervals to the data frame.
CCrw %<>% mutate (lcat.l= lencat(TL, w = 9)) # for length (mm)
CCrw %<>% mutate (lcat.g= lencat(W_g, w = 100)) # for weight (g)
CCrw %<>% mutate (lcat.lb = lencat(W_lb, w = 1))# for weight (lbs)

# Parse out by year
CCrw.2016 <- CCrw [ which(CCrw$Year=='2016'), ]
CCrw.2017 <- CCrw [ which(CCrw$Year=='2017'), ]
CCrw.2018 <- CCrw [ which(CCrw$Year=='2018'), ]

# Create length frequency tables
# Length
(freq.16len <- xtabs(~lcat.l,data = CCrw.2016))
(freq.17len <- xtabs(~lcat.l,data = CCrw.2017))
(freq.18len <- xtabs(~lcat.l,data = CCrw.2018))
(freq.plen  <- xtabs(~lcat.l, data = CCrw))
round(prop.table(freq.16len)*100,1)
round(prop.table(freq.17len)*100,1)
round(prop.table(freq.18len)*100,1)
round(prop.table(freq.plen)*100,1)

# Weight
# g
(freq.16wg <- xtabs(~lcat.g,data = CCrw.2016))
(freq.17wg <- xtabs(~lcat.g,data = CCrw.2017))
(freq.18wg <- xtabs(~lcat.g,data = CCrw.2018))
(freq.pwg  <- xtabs(~lcat.g,data = CCrw))
round(prop.table(freq.16wg)*100,1)
round(prop.table(freq.17wg)*100,1)
round(prop.table(freq.18wg)*100,1)
round(prop.table(freq.pwg)*100,1)

# lb
(freq.16wlb <- xtabs(~lcat.lb,data = CCrw.2016))
(freq.17wlb <- xtabs(~lcat.lb,data = CCrw.2017))
(freq.18wlb <- xtabs(~lcat.lb,data = CCrw.2018))
(freq.pwlb  <- xtabs(~lcat.lb,data = CCrw))
round(prop.table(freq.16wlb)*100,1)
round(prop.table(freq.17wlb)*100,1)
round(prop.table(freq.18wlb)*100,1)
round(prop.table(freq.pwlb)*100,1)

# Plot 
# Length
hist(~lcat.l,data = CCrw.2016, breaks = seq(280,800,9),xlab = "Total Length (mm)")
hist(~lcat.l,data = CCrw.2017, breaks = seq(280,800,9),xlab = "Total Length (mm)")
hist(~lcat.l,data = CCrw.2018, breaks = seq(280,800,9),xlab = "Total Length (mm)")
hist(~lcat.l,data = CCrw, breaks = seq(280,800,9),xlab = "Total Length (mm)")

# Age
hist(~Age,data = CCrw.2016, breaks = seq(1,8,1),xlab = "Age (2016)")
hist(~Age,data = CCrw.2017, breaks = seq(1,8,1),xlab = "Age (2017)")
hist(~Age,data = CCrw.2018, breaks = seq(1,8,1),xlab = "Age (2018)")
hist(~Age,data = CCrw, breaks = seq(1,8,1),xlab = "Age (Pooled)")

# Weight
# g
hist(~lcat.g,data = CCrw.2016, breaks = seq(0,5500,100), xlab = "Weight, g (2016)")
hist(~lcat.g,data = CCrw.2017,  breaks = seq(0,5500,100), xlab = "Weight, g (2017)")
hist(~lcat.g,data = CCrw.2018, breaks = seq(0,5500,100), xlab = "Weight, g (2018)")
hist(~lcat.g,data = CCrw, breaks = seq(0,5500,100), xlab = "Weight, g (Pooled)")

# lb
hist(~lcat.lb,data = CCrw.2016, breaks = seq(0,12,1), xlab = "Weight, lb (2016)")
hist(~lcat.lb,data = CCrw.2017, breaks = seq(0,12,1), xlab = "Weight,lb (2017)")
hist(~lcat.lb,data = CCrw.2018, breaks = seq(0,12,1), xlab = "Weight, lb (2018)")
hist(~lcat.lb,data = CCrw, breaks = seq(0,12,1), xlab = "Weight.lb (Pooled)")


##############################
## Fitting a Von-bert Curve ##
##############################
# Read in data
CC.pooled <- read.csv("/Users/April/Desktop/Chapter 1/Data/pooled_long.csv")

# Parse out by year
CC.2016 <- CC.pooled [ which(CC.pooled$Year=='2016'), ]
CC.2017 <- CC.pooled [ which(CC.pooled$Year=='2017'), ]
CC.2018 <- CC.pooled [ which(CC.pooled$Year=='2018'), ]

# 2016
vbt <- vbFuns("typical")
vbt.start16 <- vbStarts(BC.Len~BC.Age,data=CC.2016,type="typical")
vbt.fit16 <- nls(BC.Len~vbt(BC.Age,Linf,K,t0),data=CC.2016,start=vbt.start16)
summary(vbt.fit16)
plot(BC.Len~BC.Age,data=CC.2016,pch=16, main="2016", xlab="Back-calculated Age",
     ylab="Back-caluclated length (mm)")
curve(vbt(x,Linf=coef(vbt.fit16)),from=0,to=19,col="red",lwd=2,add=TRUE)

# 2017
vbt.start17 <- vbStarts(BC.Len~BC.Age,data=CC.2017,type="typical")
vbt.fit17 <- nls(BC.Len~vbt(BC.Age,Linf,K,t0),data=CC.2017,start=vbt.start17)
summary(vbt.fit17)
plot(BC.Len~BC.Age,data=CC.2017,pch=16, main="2017", xlab="Back-calculated Age",
     ylab="Back-caluclated length (mm)")
curve(vbt(x,Linf=coef(vbt.fit17)),from=0,to=19,col="red",lwd=2,add=TRUE)

# 2018
vbt.start18 <- vbStarts(BC.Len~BC.Age,data=CC.2018,type="typical")
vbt.fit18 <- nls(BC.Len~vbt(BC.Age,Linf,K,t0),data=CC.2018,start=vbt.start18)
summary(vbt.fit18)
plot(BC.Len~BC.Age,data=CC.2018,pch=16, main="2018", xlab="Back-calculated Age",
ylab="Back-caluclated length (mm)")
curve(vbt(x,Linf=coef(vbt.fit18)),from=0,to=19,col="red",lwd=2,add=TRUE)

# Pooled

vbt.start <- vbStarts(BC.Len~BC.Age,data=CC.pooled,type="typical")
vbt.fit <- nls(BC.Len~vbt(BC.Age,Linf,K,t0),data=CC.pooled,start=vbt.start16)
summary(vbt.fit)
plot(BC.Len~BC.Age,data=CC.pooled,pch=16, main="2016", xlab="Back-calculated Age",
     ylab="Back-caluclated length (mm)")
curve(vbt(x,Linf=coef(vbt.fit)),from=0,to=19,col="red",lwd=2,add=TRUE)

###########################
# Plotting Growth by year #
###########################
# Growth by year for age 2 fish
y2 <- CC.pooled[ which(CC.pooled$BC.Age=='2'), ]
y2$Growth.Year <- as.factor(y2$Growth.Year)
violin1 <- ggplot(y2, aes(x=Growth.Year, y=Growth)) + 
  geom_violin(trim=FALSE, fill='azure3', color="darkslategray")+
  geom_boxplot(width=0.07) + theme_classic()+
  labs(title="Growth by year for age 2 fish", x="Year", y = "Growth")
plot(violin1)

# Growth by year for age 3 fish
y3 <- CC.pooled[ which(CC.pooled$BC.Age=='3'), ]
y3$Growth.Year <- as.factor(y3$Growth.Year)
violin2 <- ggplot(y3, aes(x=Growth.Year, y=Growth)) + 
  geom_violin(trim=FALSE, fill='azure3', color="darkslategray")+
  geom_boxplot(width=0.07) + theme_classic()+
  labs(title="Growth by year for age 3 fish", x="Year", y = "Growth")
plot(violin2)

# Growth by year for age 4 fish
y4 <- CC.pooled[ which(CC.pooled$BC.Age=='4'), ]
y4$Growth.Year <- as.factor(y4$Growth.Year)
violin3 <- ggplot(y4, aes(x=Growth.Year, y=Growth)) + 
  geom_violin(trim=FALSE, fill='azure3', color="darkslategray")+
  geom_boxplot(width=0.07) + theme_classic()+
  labs(title="Growth by year for age 4 fish", x="Year", y = "Growth")
plot(violin3)

# Growth by year for age 5 fish
y5 <- CC.pooled[ which(CC.pooled$BC.Age=='5'), ]
y5$Growth.Year <- as.factor(y5$Growth.Year)
violin4 <- ggplot(y5, aes(x=Growth.Year, y=Growth)) + 
  geom_violin(trim=FALSE, fill='azure3', color="darkslategray")+
  geom_boxplot(width=0.07) + theme_classic()+
  labs(title="Growth by year for age 5 fish", x="Year", y = "Growth")
plot(violin4)

# Fish aged 2-4
data<- subset(CC.pooled, BC.Age>=2 & BC.Age<=4 & Growth.Year>=2013)
data$Growth.Year <- as.factor(data$Growth.Year)
data$BC.Age <- as.factor(data$BC.Age)
violin5 <- ggplot(data, aes(x=Growth.Year, y=Growth, fill = BC.Age), scale=Growth) + 
  geom_violin(trim=FALSE, position=position_dodge(1))+ 
  geom_boxplot(width=0.07, position=position_dodge(1)) +
  labs(title="Growth by year", x="Year", y = "Growth")
plot(violin5)

# All fish on one plot
data2<- subset(CC.pooled, Growth.Year>=2013)
data2$Growth.Year <- as.factor(data2$Growth.Year)
data2$BC.Age <- as.factor(data2$BC.Age)
violin6 <- ggplot(data2, aes(x=Growth.Year, y=Growth, fill = BC.Age), scale=Growth) + 
  geom_violin(trim=FALSE, position=position_dodge(1)) + facet_wrap(~BC.Age) + 
  geom_boxplot(width=0.07, position=position_dodge(1)) + 
  labs(title="Growth by year", x="Year", y = "Growth")
plot(violin6)

###########################
## PSD & Relative Weight ##
###########################

# Fit a linear regression to get weight:length coefficients
CCrw_reg <- lm(logW~logL, CCrw); summary(CCrw_reg)
# Standard weight for Common Carp using Total Length
wsVal("Common Carp")
# Find minimin Total Length used for this standard weight
(wsCC <- wsVal("Common Carp", simplify = TRUE))

# Extract the coefficients for standard weight below
wsCC[["int"]]
wsCC[["slope"]]
wsCC[["quad"]] #No quadratic term used

CCrw %<>% mutate(Ws=10^(wsCC[["int"]]+wsCC[["slope"]]*logL),
                 Wr=W_g/Ws*100)

# Full list of relative weights for all years
CCrw

# Now gcats- across all fish
CCrw1 <- CCrw %>%
  filter(TL>=psdVal("Common Carp")) %>%
  mutate(gcat=lencat(TL,breaks=psdVal("Common Carp"),
                     use.names=TRUE, droplevels = TRUE))

# Look only at 2016 for indiviudals within the range of acceptable lengths
CCrw2016 <- CCrw %>%
  mutate(gcat=lencat(TL,breaks=psdVal("Common Carp"),
                     use.names=TRUE)) %>%
  filterD(Year==2016)

# 2017
CCrw2017 <- CCrw %>%
  mutate(gcat=lencat(TL,breaks=psdVal("Common Carp"),
                     use.names=TRUE)) %>%
  filterD(Year==2017)

# 2018
CCrw2018 <- CCrw %>%
  mutate(gcat=lencat(TL,breaks=psdVal("Common Carp"),
                     use.names=TRUE)) %>%
  filterD(Year==2018)

# create table of freq of fish in each length category
(gfreq <- xtabs(~gcat, data = CCrw1))
(gfreq.2016 <- xtabs(~gcat, data = CCrw2016))
(gfreq.2017 <- xtabs(~gcat, data = CCrw2017))
(gfreq.2018 <- xtabs(~gcat, data = CCrw2018))

# PSD
(psdXY1 <- prop.table(gfreq)*100)
(psdXY2 <- prop.table(gfreq.2016)*100)
(psdXY3 <- prop.table(gfreq.2017)*100)
(psdXY4 <- prop.table(gfreq.2018)*100)

# PSD-X
(psdX1 <- rcumsum(psdXY1)) # pooled
(psdX2 <- rcumsum(psdXY2)) # 2016
(psdX3 <- rcumsum(psdXY3)) # 2017
(psdX4 <- rcumsum(psdXY4)) # 2018

# Summarize data
Summarize(Wr~gcat,data=CCrw1,digits=0)# pooled
Summarize(Wr~gcat,data=CCrw2016,digits=0)# 2016
Summarize(Wr~gcat,data=CCrw2017,digits=0)# 2017
Summarize(Wr~gcat,data=CCrw2018,digits=0)# 2018

# Kruskal-Wallace test for multiple comparisons Dunn et al 1964
dunnTest(Wr~Year, data=CCrw1, method="holm") # if wr is different among years
dunnTest(Wr~gcat, data=CCrw1, method="holm") # if wr is different among length categories

###############
## Mortality ##
###############
# Create data frame for 2016 data. This is the cross-sectional catch for 2016
bkt16<- data.frame(age=0:7,ct=c(0,1,14,56,65,36,15,2)) 

# Catch-curve regression
thcc <- catchCurve(ct~age,data=bkt16,ages2use=4:7)
summary(thcc)
confint(thcc)

# Weighted catch-curve regression
thcc.2 <- catchCurve(ct~age,data=bkt16,ages2use=4:7,use.weights=TRUE)
summary(thcc.2)
confint(thcc.2)

# ChapmanRobson
thcr <- chapmanRobson(ct~age,data=bkt16,ages2use=4:7)
summary(thcr)
confint(thcr)

########################################
## Mean Back-calculated length-at-age ##
########################################

# Parse out by year
# 2016
Age2.2016 <- CC2016 [ which(CC.2016$BC.Age=='2'), ]
Age3.2016 <- CC.2016 [ which(CC.2016$BC.Age=='3'), ]
Age4.2016 <- CC.2016 [ which(CC.2016$BC.Age=='4'), ]
Age5.2016 <- CC.2016 [ which(CC.2016$BC.Age=='5'), ]

# 2017
Age2.2017 <- CC.2017 [ which(CC.2017$BC.Age=='2'), ]
Age3.2017 <- CC.2017 [ which(CC.2017$BC.Age=='3'), ]
Age4.2017 <- CC.2017 [ which(CC.2017$BC.Age=='4'), ]
Age5.2017 <- CC.2017 [ which(CC.2017$BC.Age=='5'), ]

# 2018
Age2.2018 <- CC.2018 [ which(CC.2018$BC.Age=='2'), ]
Age3.2018 <- CC.2018 [ which(CC.2018$BC.Age=='3'), ]
Age4.2018 <- CC.2018 [ which(CC.2018$BC.Age=='4'), ]
Age5.2018 <- CC.2018 [ which(CC.2018$BC.Age=='5'), ]

# Pooled
Age2 <- CC.pooled [ which(CC.pooled$BC.Age=='2'), ]
Age3 <- CC.pooled [ which(CC.pooled$BC.Age=='3'), ]
Age4 <- CC.pooled [ which(CC.pooled$BC.Age=='4'), ]
Age5 <- CC.pooled [ which(CC.pooled$BC.Age=='5'), ]

# Kruskal-Wallace test for multiple comparisons Dunn et al 1964
dunnTest(Growth~Cohort, data=Age2, method="holm") # if wr is different among years
dunnTest(Wr~gcat, data=CCrw1, method="holm") # if wr is different among length categories


######################################
## Abundance from Capture-Recapture ##
######################################

# Read in data
biomass <-read.csv("/Users/April/Desktop/Chapter 1/Data/Carp_rodeo_Weight.csv")

# Abundance from a single, closed population capture-recapture with small sample size
carp_recap <- mrClosed(200,207,12)
summary(carp_recap, incl.SE=TRUE)
confint(carp_recap,verbose=TRUE)

# Biomass
summary(biomass)

# Calculate the mean
mean(biomass$Weight)

# Calculate the variance
var(biomass$Weight)

# Calculate the standard deviation 
sd(biomass$Weight)

# Find the estimated standard error
sd(biomass$Weight) / sqrt(length(biomass$Weight))

# Calculate a 95% CI
lower <- 2093.015 - (49.02402* 1.96) 
upper <- 2093.015 + (49.02402* 1.96)
( CI.95 <- c(lower,upper) )

# Extrapolate to lake








