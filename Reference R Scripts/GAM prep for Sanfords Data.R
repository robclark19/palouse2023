# packages used
library("ggplot2")
library("tidyverse")
library("daymetr")
library("mgcv")


# Outline ####
# 1 how to calculate species-specific phenology based on GDD 

# 2 how to extract location-specific temperature data from daymet database

# 3 example calculation for a single site in the 12-year data

# 4 example GAM without real data

# Goals ######
# We want to be able to run a GAM or regression on the relationship 
# between alate counts and the predicted peak phenology of alate emergence based on
# a priori predictions from phenology models (PM)
# I hypothesized that vetch PM *at low elevations* would be better than pea aphid PM at farms
# this hypothesis is tested with 10+ years of discontinuous pan-trap data from U.of Idaho

# coding questions are highlighted with "??"



# Part 1 ####

# calculating species specific GDD (cumulative degree days for a certain organism)

# made up dataset of 30 days and daily temps
days <- c(1:30)
temps <- c(60, 65, 70, 90, 90, 70, 65, 60, 85, 85, 82, 50, 70, 80, 50, 60, 75, 82, 82, 80, 60, 75, 82, 82, 80, 60, 85, 85, 82, 50)
plot(days, temps)

# ok each row has to add up the current day over 50, plus the previous one


# at what temp does development stop? that is the lower bound of the PM
# these #'s have to be able to be substituted using functions that are input (like through shiny)
# too cold for development
lower.bound <- 41.9
# too hot for development
upper.bound <- 82.4


# Developmental thresholds from Campbell, A. and M. Mackauer. 1975.
# http://ipm.ucanr.edu/PHENOLOGY/ma-pea_aphid.html
# Lower:	41.9?F	( 5.5?C)
# Upper:	82.4?F	(28.0?C)

# a conditional statement that does not add degree days when its too hot (development stops)
temps <- if_else(temps > upper.bound, true=lower.bound, false=temps*1)
temps <- if_else(temps < lower.bound, true=lower.bound, false=temps*1)

# then we want to add all the degree days above the lower bound
# "hot days" zero out since they are set to equal to the lower bound


# ?? ###
# How do I set it so instead of zeroing out, at the hot temp they just cap out

# calculations for degree days
ac.dd <- temps %>%  -lower.bound %>% cumsum() 

# plot as a time series 
plot(ts(ac.dd), ylab="Cumulative Degree Days", xlab="Days in May")


# OK what if we use the actual values for pea aphids?
# http://ipm.ucanr.edu/PHENOLOGY/ma-pea_aphid.html
# Degree-day accumulations required for each stage of development
 
#Form: Apterous	DD (?F)	DD (?C)
#Generation time (nymph to adult):	197.1	109.5
#Form: Alate	DD (?F)	DD (?C)
#Generation time (nymph to adult):	214.0	118.9


# Egg mass eclosion:	507.6
# 50% pupation:	925-1186
alate.dd <- 214.0


# alate.variance.. how do you handle variation in development?
# do you add a function here that provides variance to the pupation or do you add it later?

# calculate when alates are formed, which occurs when the accumulative degree days 
# is above the alate formation time point of 214 accumulative degree days
fraction.alates <- if_else(ac.dd > alate.dd, true=1, false=0)


# when does egg mass eclosion occur? When we reach 507 cdd
# that should be multiplied by the variance in estimates of daily temps (or some other range)

plot(ts(fraction.alates), ylab="Proportion of Pea Aphids becoming Alates", xlab="Days")

# ok so after 12 days, 100% of the nymphs you started with now formed alates


# QQs #
# We need a way to automatically input the days and temps from every single site in the
# ten year aphid dataset

# we need to find a way to model variance in temp or developmental thresholds




# Part 2 #####
# get daily temp values for our specific sites in eastern WA using daymet
# daymet is a database for 20 years of data from 2020 and earlier that gives
# historical data based on interpolation for any "pixel" on its map (https://daymet.ornl.gov/)

df.pcf <- download_daymet(site = "Palouse Conservation Farm",
                      lat = 46.7593,
                      lon = -117.1937,
                      start = 2013,
                      end = 2019,
                      internal = TRUE,
                      simplify = TRUE)

# show a plot of the time points from the downloaded data (daily temps in C for 7 years)

df.pcf %>%
  mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
  filter(measurement == "tmax..deg.c.") %>%
  ggplot() +
  geom_line(aes(x = date, y = value))


# Part 3 ####

# find the daily temps for a given site in the ten-year data from U of Idaho

aphid.time.dat <- read.csv("Aphid.Records2.csv")
head(aphid.time.dat)

# Snackweevil ####

aphid.time.dat <- read.csv("CDD_ResultsV3.csv")
head(aphid.time.dat)

# Simple GAM ####

## minmax fxn

gam_minmax <- function(mod) {
  
  fitmin <- min(mod$fitted.values)
  fitmax <- max(mod$fitted.values)
  
  inmin_index <- which.min(mod$fitted.values)
  inmax_index <- which.max(mod$fitted.values)
  
  df <- cbind(stat=c("min", "max"), 
              rbind(mod$model[inmin_index,], 
                    mod$model[inmax_index,]), 
              fit=c(fitmin, fitmax))
  
  return(df)
  
}
# added april 24 2020
# simple GAM for julian days and aphid counts

aphid.time.dat <- filter(aphid.time.dat, Julian_week < 41)

plot(aphid.time.dat$Julian_week,aphid.time.dat$AphidCount)
# huh, big chunk of missing values around 35 to 40 weeks

#ok Sanford said thats for winter pea, so remove everything before julian week 41
# 41 is when fall-planted pea aphid sampling started


#unrestricted
adt.gam <- gam(AphidCount ~ s(Julian_Day) + Year, data=aphid.time.dat)
plot(adt.gam)
summary(adt.gam)

#function taken from pea value script to give max values
gam_minmax(adt.gam)

# ok do this but with aphid DD
plot(aphid.time.dat$Aphids_DD, log(aphid.time.dat$AphidCount + 1))
aphid.time.dat$logAphidCount <- log(aphid.time.dat$AphidCount + 1)

# messy exponential dist

add.gam <- gam(logAphidCount ~ s(Aphids_DD), method="REML", data=aphid.time.dat)
summary(add.gam)
plot(add.gam, xlab="Cumulative Degree Days", ylab="Deviation in pea aphid abundance")
abline(v=700, col="red")
#abline(v=1200, col="red")
gam_minmax(add.gam)

# need to add a line where vetch flowering occurs on this scale?


gam_y <- add.gam
data.frame <- aphid.time.dat
data.frame$x <- aphid.time.dat$Aphids_DD
data.frame$y <- log(aphid.time.dat$AphidCount + 1)

gam.check(gam_y)

# x_new <- seq(0, max(x), length.out = 100)

data.frame$y_pred <- predict(gam_y)

alate.gam.plot <- ggplot(data.frame, aes(x, y)) + geom_point() + geom_smooth(method = "gam", formula = y_pred ~s(x)) +
  theme_bw(base_size=16) +
  ylab("Log pea aphid alates") +
  xlab("Cumulative Degree days") +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.title.x=element_blank()) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

alate.gam.plot




# same for peas and vetch

pdd.gam <- gam(logAphidCount ~ s(Pea_DD) + s(Elevation) + s(Latitude,Longitude), family=nb, data=aphid.time.dat)
plot(pdd.gam)
gam_minmax(pdd.gam)
summary(pdd.gam)

vdd.gam <- gam(logAphidCount ~ s(Vetch_DD), data=aphid.time.dat)
plot(vdd.gam)
gam_minmax(vdd.gam)


#
Box.test(aphid.time.dat$AphidCount)

# still sig
resid(adt.gam) %>% Box.test()

# restricted to least number of knots
adt.gam.2 <- gam(AphidCount ~ s(Julian_week, k=3), data=aphid.time.dat)
summary(adt.gam)
plot(adt.gam.2)

resid(adt.gam.2) %>% Box.test()





# Part 3a #####

# pick a random day that had a lot of alates:

arrange(aphid.time.dat, desc(AphidCount)) %>% head()

# ok 2009-Site-05, julian day 153, aphid count 87

single.dat <- aphid.time.dat %>% filter(AphidCount==87)


# this is one day + site in 2009, but we want the daymet daily temps

df.single <- download_daymet(site = "47.16 by -117.26",
                          lat = 47.16013,
                          lon = -117.2661,
                          start = 2009,
                          end = 2009,
                          internal = TRUE,
                          simplify = TRUE)

# qq

# need to extract just the daily temps for yday 1-153 (153 is when the sampling occurred)

# the structure of these data are confusing... how do i only select temps?


# once we get a simple data table of julian days and daily temps..
# feed that into the Part 1 function and that will give you a single cumulative degree day
# for that day and location the alates were collected

df.single.short <- df.single %>% filter(measurement == "tmax..deg.c.")

str(df.single.short)


#convert to F because I'm a damned yankee and just make a vector

# (0?C ? 9/5) + 32 = 32?F

temps <- (df.single.short$value * (9/5) + 32)
temps



# modified aphid code from above
lower.bound <- 41.9
upper.bound <- 82.4

# a conditional statement that does not add degree days when its too hot (development stops)
temps <- if_else(temps > upper.bound, true=lower.bound, false=temps*1)
temps <- if_else(temps < lower.bound, true=lower.bound, false=temps*1)

# calculations for cumulative degree days
ac.dd <- temps %>%  -lower.bound %>% cumsum() 

# plot as a time series 
plot(ts(ac.dd), ylab="Cumulative Degree Days for Pea Aphids", xlab="Julian Days")



# below aphid nymphs starting that year should have come out on Julian day 99
# not useful but also not useless
fraction.alates <- if_else(ac.dd > alate.dd, true=1, false=0)
plot(ts(fraction.alates), ylab="Proportion of Pea Aphids becoming Alates", xlab="Days")



# cool stuff. now pull out the cdd at julian day 154 in 2009

yankee.dd <- ac.dd[154]
yankee.dd

# ok so yankee.dd is 1369.512, so that means aphids had that much cdd
# when that pan trap was sampled


# Part 4 ####

# bring it all together #

# what we want is a dataframe that is a modified version of the long-term aphid dataset

head(aphid.time.dat)

# after aphid count are the growing degree day values for every single day and site

# however, there are multiple ways to calculate growing degree days from phenology models

# we can use the 1973 aphid paper, the GDD for pea, the GDD for vetch, 
# we can also use other locations where aphids may come from
# therefore we want the GDD for vetch at LOW elevations and the GDD for aphids too

# we should have a set of functions that lets us easily plug in different phenology model values
# so that not all of these have to computed by hand
