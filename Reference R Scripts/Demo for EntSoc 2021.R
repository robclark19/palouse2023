
# Project Pea Value - using vetch to forecast pea aphid outbreaks 2018-2020 surveys
# This is the full script for the 2018-2020 experiment demonstrating PEMV and aphids moving among
# hairy vetch populations and pea plants over 3 years
# preliminary analyses are to inform manuscript outlines

# to do: 
# map figures
# finish GAMs with time series approach
# make growing degree day models with ten-year data

# packages #####
# library("lme4")
# library("car")
# library("multcomp")
library("ggplot2")
# library("emmeans")
# library("multcompView")
# library("glmmTMB")
library("xlsx")
# library("tseries")  
library("mgcv")
library("tidymv")

#mapping tools
library("tidyverse")
library("OpenStreetMap")
library("ggpubr")
# library(cowplot)
# library(ggrepel)
# library(viridis)

# Fig 1 Map '18,'19,'20 #####

# Code imported from Baikal paper ####
# Input site level data

aphidsites.dat <- read.xlsx("pea value sites.xlsx", 1, header=TRUE)
str(aphidsites.dat)


# Get a zoomed in map
base_map_zoom <- openmap(upperLeft = c(46.25, -117.65),
                         lowerRight = c(47.2, -116.75),
                         type = "stamen-terrain", zoom=10) %>%
  openproj()

# type argument for sat images is "bing"

# Build a close up map with satellite imagery & zoomed in map
zoom_map <- autoplot.OpenStreetMap(base_map_zoom) +
  geom_point(data = aphidsites.dat,
             aes(x = Long, y = Lat,
                 color = as.factor(Year),
                 shape = Location.Type),
             size=3) +
  scale_size_continuous(10) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("RED", "Blue", "Dark Green"))
  
zoom_map

# This plot is associated with Figure 1 in the accompanying manuscript.
# save map ####

 ggsave(filename = "Figure 1.png", plot = zoom_map, device = "png",
       width = 9, height = 4.5, units = "in")



# 2020 ####

# quick pan trap dataset
# these data are collected from 272 events among ~14 pan traps by the snake river
ptp.dat <- read.csv("pan trap 2020 pilot.csv")
ptp.dat$time.block <- as.factor(ptp.dat$Sampling.Period)

str(ptp.dat)

# simple GLM to look at pea aphid counts among days
pan.pea.glm <- glm.nb(Pea.Aphids ~ time.block, data=ptp.dat)
Anova(pan.pea.glm)

pan.pea.lsm <- cld(emmeans(pan.pea.glm, ~ time.block), sort=FALSE,type="response")
pan.pea.lsm

pan.pea.plot1 <- ggplot(data=pan.pea.lsm, aes(x = time.block, y=response)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=response-(SE/2), ymax=response+(SE/2), width=0)) +
  geom_line(aes(group=time.block), size=1) +
  ylab("Total aphids (# per plant)") +
  xlab("Duration (72 hour sampling periods)") +
  labs(title = "Pea aphid alate counts in traps") +
  theme(legend.position="bottom")
pan.pea.plot1

plot_model(pan.pea.glm, type = "pred", terms = c("time.block"))

# non-pea aphids
non.pea.glm <- glm.nb(Other.Aphids ~ time.block, data=ptp.dat)
plot_model(non.pea.glm, type = "pred", terms = c("time.block"))

# all aphids
ptp.dat$All.Aphids <- ptp.dat$Pea.Aphids + ptp.dat$Other.Aphids
all.pea.glm <- glm.nb(All.Aphids ~ time.block, data=ptp.dat)
plot_model(all.pea.glm, type = "pred", terms = c("time.block"))

# simple model to look at other, non-pea aphids
# different overall trend here
non.pea.glm <- glm.nb(Other.Aphids ~ time.block, data=ptp.dat)
Anova(non.pea.glm)

plot_model(non.pea.glm, type = "pred", terms = c("time.block"))


# sweep aphid quick pilot data

vsa.dat <- read.csv("veth sweep 2020 pilot.csv")
str(vsa.dat)

vsa.dat$time.block <- as.factor(vsa.dat$Sampling.Period)
vsa.dat$Sampling.Period <- as.numeric(vsa.dat$Sampling.Period)

#glm on aphid abundance on vetch predicted by time block and vetch traits
sweep.pea.glm <- glm.nb(Aphid.Count ~ Average.Cover + Flower.Density 
                        + Seed.Pod.Density + Site*Vetch.Period, data=vsa.dat)
Anova(sweep.pea.glm)


# investigate main effects on aphid abundance
plot_model(sweep.pea.glm)
plot_model(sweep.pea.glm, type = "pred", terms = c("Average.Cover"))
plot_model(sweep.pea.glm, type = "pred", terms = c("Flower.Density"))
plot_model(sweep.pea.glm, type = "pred", terms = c("Seed.Pod.Density"))
plot_model(sweep.pea.glm, type = "pred", terms = c("Site"))
plot_model(sweep.pea.glm, type = "pred", terms = c("Vetch.Period"))
plot_model(sweep.pea.glm, type = "int", terms = c("Site"))


sweep.pea.lsm <- cld(emmeans(sweep.pea.glm, ~ Site*Vetch.Period), sort=FALSE)
sweep.pea.lsm

sweep.pea.plot1 <- ggplot(data=sweep.pea.lsm, aes(x = Site, y=emmean, color=Vetch.Period)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=emmean-(SE/2), ymax=emmean+(SE/2), width=0)) +
  # geom_line(aes(group=Vetch.Period), size=1) +
  # ylab("Total aphids (# per plant)") +
  # xlab("Duration (72 hour sampling periods)") +
  # labs(title = "Pea aphid alate counts in traps") +
  theme(legend.position="bottom")
sweep.pea.plot1


# are sites synchronous?
sweep.pea.glm.2 <- glm.nb(Aphid.Count ~ Site*time.block, data=vsa.dat)
Anova(sweep.pea.glm.2)

sweep.pea.lsm.2 <- cld(emmeans(sweep.pea.glm.2, ~ Site*time.block), sort=FALSE)
sweep.pea.lsm.2

# plot for synchrony
sweep.pea.plot2 <- ggplot(data=sweep.pea.lsm.2, aes(x = time.block, y=emmean, color=Site)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=emmean-(SE/2), ymax=emmean+(SE/2), width=0)) +
  geom_line(aes(group=Site), size=1) +
  # ylab("Total aphids (# per plant)") +
  # xlab("Duration (72 hour sampling periods)") +
  # labs(title = "Pea aphid alate counts in traps") +
  theme(legend.position="bottom")
sweep.pea.plot2

# 2020 GAM ####
# sweep.2020.gam <- gam(vsa.dat$Aphid.Count>0~s(vsa.dat$time.block),family=gaussian)

gam_y <- gam(vsa.dat$Aphid.Count ~ s(as.numeric(vsa.dat$time.block)), method = "REML", family=nb)
summary(gam_y)
plot(gam_y, which = 1)

x <- as.numeric(vsa.dat$time.block)
y <- log(vsa.dat$Aphid.Count+1)


# https://stats.stackexchange.com/questions/137109/selecting-knots-for-a-gam
# select K for "knots"
gam_y <- gam(y ~ s(x, k=4), method = "REML")
plot(gam_y)


gam.check(gam_y)

# x_new <- seq(0, max(x), length.out = 100)

y_pred <- predict(gam_y, data.frame(x))

vetch.gam.plot <- ggplot(vsa.dat, aes(x, y)) + geom_point() + geom_smooth(method = "gam", formula = y ~s(x)) +
  theme_bw(base_size=16) +
  ylab("Log aphid abundance per 10 meter sweep") +
  xlab("Sampling Period (4 day increments") +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.title.x=element_blank()) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  vetch.gam.plot
  
  
  
  
## Vetch Aphids 20 #####
  # vetch aphid counts in 2020
  x1 <- as.numeric(vsa.dat$Sampling.Period)
  y1 <- log(vsa.dat$Aphid.Count+1)
  
  vetch.aphids.2020<- gam(y1 ~ s(x1, k=4), method = "REML")
  
  log(vsa.dat$Aphid.Count+1) %>% Box.test()
 
  resid(vetch.aphids.2020) %>% Box.test()
  
  summary(vetch.aphids.2020)
  
  
  # make model predictions the same length as the original dataset (so you can map on raw data/dates)
  model_p <- predict_gam(vetch.aphids.2020, length_out = 287)
  
  # make the model output in ggplot
  pgam_plot <- model_p %>%
    ggplot(aes(x1, fit)) +
    geom_smooth_ci(ci_alpha=.3)
  
  
  # plots points and labels for the correct original data
  vetch.aphids.2020.ggplot <- pgam_plot +
    geom_point(aes(y=y1)) +
    scale_x_continuous(name="2020 Sampling Periods", breaks = c(0,5,10,15,20)) +
    scale_y_continuous(name="# of Aphids on Transect") +
    theme_bw(base_size=12)
  
  vetch.aphids.2020.ggplot 
  
  gam_minmax(vetch.aphids.2020)
  
  
  
  
  ##Pan Aphids 20 ####
  
  full_2020_dat <- read.xlsx("time series 2019 and 2020 final.xlsx", 3, header=TRUE)
  str(full_2020_dat)
  
  
  x2 <- as.numeric(full_2020_dat$Sampling.Period)
  y2 <- full_2020_dat$Pan.Trap.Pea.Aphid.Count
  # z <- full_2020_dat$Sweep.Site.Name

  pan.aphids.2020 <- gam(y2 ~ s(x2, k=3), method = "REML")
  
  y2 %>% Box.test()
  
  resid(pan.aphids.2020) %>% Box.test()
  
  summary(pan.aphids.2020)
  
  plot(pan.aphids.2020)
  
  # make model predictions the same length as the original dataset (so you can map on raw data/dates)
  model_p <- predict_gam(pan.aphids.2020, length_out = 365)
  
  # make the model output in ggplot
  pgam_plot <- model_p %>%
    ggplot(aes(x2, fit)) +
    geom_smooth_ci(ci_alpha=.3)
  
  
  # plots points and labels for the correct original data
  pan.aphids.2020.ggplot <- pgam_plot +
    geom_point(aes(y=y2)) +
    scale_x_continuous(name="2020 Sampling Periods") +
    scale_y_continuous(name="# of Aphids in Pan Traps") +
    theme_bw(base_size=12)
  
  pan.aphids.2020.ggplot 
  
  # what are the mix max points of this model?
  gam_minmax(pan.aphids.2020)
  
  ## 2020 ggarrange ####
  
  
  figure.2020.all <- ggarrange(pan.aphids.2020.ggplot,vetch.aphids.2020.ggplot,
                               labels = c("", ""), ncol = 1, nrow = 2)
  figure.2020.all
  

  
  
  
  
  
# sanfords 2020 pan trap data GAM


sanford_2020_dat <- read.csv("Aphid.Records2.csv") %>% filter(Year == 2020)
 
str(sanford_2020_dat)


gam_san <- gam(sanford_2020_dat$AphidCount ~ s(as.numeric(sanford_2020_dat$Julian_week), k=10), method = "REML")
summary(gam_san)
plot(gam_san)


x <- as.numeric(sanford_2020_dat$Julian_week)
y <- log(sanford_2020_dat$AphidCount + 1)

gam_san <- gam(y ~ s(x), method = "REML")
plot(gam_san)

gam.check(gam_san)
  

#plot for pan_2020_dat

x_new <- seq(0, max(x), length.out = 100)
y_pred <- predict(gam_san, data.frame(x = x_new))

san.gam.plot <- ggplot(sanford_2020_dat, aes(x, y)) + geom_point() + geom_smooth(method = "gam", formula = y ~s(x)) +
  theme_bw(base_size=16) +
  ylab("Alate counts in pan traps") +
  xlab("Sampling Period (Julian Week)") +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.title.x=element_blank()) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

san.gam.plot


# this is the money plot to compare to pan trap data

plot_model(sweep.pea.glm, type = "pred", terms = c("time.block"))


# quick look at the histogram (LOTS of zeroes of course)
hist(vsa.dat$Aphid.Count, breaks = 100)



# pan-vetch merge #####
# this has the averages calculated by Gabi for 2020

panvetch.dat <- read.xlsx("time series 2019 and 2020 final.xlsx", 4, header=TRUE)
str(panvetch.dat)

ccf(panvetch.dat$Vetch.Aphid.Count, panvetch.dat$Pan.Trap.Pea.Aphid.Count, plot=TRUE)

vetch.time <- ts(panvetch.dat$Vetch.Aphid.Count, frequency = 1)
plot(vetch.time)

pan.time <- ts(panvetch.dat$Pan.Trap.Pea.Aphid.Count, frequency = 1)
plot(pan.time)


# holt winters with these guys (smoothing), taken from time series workshop
# not much to smooth with only 14 observations
# hwforecasts <- HoltWinters(vetch.time, gamma=FALSE)
# hwforecasts
# high alpha means very short memory for both slope of the trend and the current level
# plot(hwforecasts)

# some other stuff using plant traits
plot(ts(panvetch.dat$Average.Vetch.Coverage))
plot(ts(panvetch.dat$Vetch.Average.Seed.Pods))
plot(ts(panvetch.dat$Vetch.Average.Flowe.Density))

#
ccf(panvetch.dat$Vetch.Average.Flowe.Density,panvetch.dat$Pan.Trap.Pea.Aphid.Count,plot=TRUE)

# are non-pea and pea aphid arrivals in pan traps correlated (yes at time step 0 and +2)
ccf(panvetch.dat$Pan.Trap.Non.pea.Aphid.Count, panvetch.dat$Pan.Trap.All.Aphid.Count, plot=TRUE)

# can we overlay some plots?
# so far this is pretty messy
# https://stackoverflow.com/questions/12402594/how-to-plot-overlaying-time-series-in-r

comb_ts <- cbind(panvetch.dat$Average.Vetch.Coverage, panvetch.dat$Vetch.Average.Flowe.Density) # please make sure the length of both your timeseries
plot.ts(comb_ts, plot.type = "single")











# 2019 #####

time2019.dat <- read.xlsx("time series 2019 and 2020 final.xlsx", 2, header=TRUE)
str(time2019.dat)

# exclude non-snake river sites in future analysis?



# some other stuff using plant traits
plot(ts(time2019.dat$Vetch.Average.Coverage))
plot(ts(time2019.dat$Vetch.Average.Seed.Pods))
plot(ts(time2019.dat$Vetch.Average.Flowers))
plot(ts(time2019.dat$Sweep.Aphid.Total.Count))
plot(ts(time2019.dat$Pea.Average.Height))
plot(ts(time2019.dat$Pan.Trap.Pea.Aphids.Total.Count))

ccf(time2019.dat$Sweep.Aphid.Total.Count,time2019.dat$Pan.Trap.Pea.Aphids.Total.Count)



# 2019 GAM #####
#rewrite with full dataset
time2019.dat <- read.xlsx("time series 2019 and 2020 final.xlsx", 1, header=TRUE)
str(time2019.dat)


## Vetch Aphids 19 #####
# vetch aphids
x <- as.numeric(time2019.dat$Sampling.Period)
y <- time2019.dat$Sweep.Aphid.Total.Count

# try cutting out the farm sites and or vetch sites?
# an original iteration of the model had a regression term:
# + time2019.dat$Pan.Trap.Pea.Aphids.Total.Count

vetch.aphids.2019 <- gam(y ~ s(x), method = "REML")
summary(vetch.aphids.2019)

resid(vetch.aphids.2019) %>% Box.test()

# Not included in 2019 figure due to insanely low aphid counts in 2019




## Vetch Cover 19 ####
# vetch coverage
x <- as.numeric(time2019.dat$Sampling.Period)
y <- time2019.dat$Vetch.Average.Coverage

vetch.cover.2019 <- gam(y ~ s(x), method = "REML")
plot(vetch.cover.2019)
resid(vetch.cover.2019) %>% Box.test()

summary(vetch.cover.2019)


# make model predictions the same length as the original dataset (so you can map on raw data/dates)
model_p <- predict_gam(vetch.cover.2019, length_out = 258)

# make the model output in ggplot
pgam_plot <- model_p %>%
  ggplot(aes(x, fit)) +
  geom_smooth_ci(ci_alpha=.3)


# plots points and labels for the correct original data
vetch.cover.2019.ggplot <- pgam_plot +
  geom_point(aes(y=time2019.dat$Vetch.Average.Coverage)) +
  scale_x_continuous(name="2019 Sampling Periods", breaks = c(0,5,10,15,20)) +
  scale_y_continuous(name="Coverage of Hairy Vetch on transect (%)") +
  theme_bw(base_size=12)

vetch.cover.2019.ggplot 








## Vetch Flowers 19 ####
x <- as.numeric(time2019.dat$Sampling.Period)
y <- time2019.dat$Vetch.Average.Flowers

vetch.flower.2019 <- gam(y ~ s(x, k=4), method = "REML")
summary(vetch.flower.2019)

# ok so this is NOT a good model unless you cut it down to k=4
resid(vetch.flower.2019) %>% Box.test() 

# not a temporal pattern in these residuals huh
plot(resid(vetch.flower.2019))
# 4 is the highest number of knots needed

# make model predictions the same length as the original dataset (so you can map on raw data/dates)
model_p <- predict_gam(vetch.flower.2019, length_out = 258)

# make the model output in ggplot
pgam_plot <- model_p %>%
  ggplot(aes(x, fit)) +
  geom_smooth_ci(ci_alpha=.3)


# plots points and labels for the correct original data
vetch.flowers.2019.ggplot <- pgam_plot +
  geom_point(aes(y=time2019.dat$Vetch.Average.Flowers)) +
  scale_x_continuous(name="2019 sampling periods", breaks = c(0,5,10,15,20)) +
  scale_y_continuous(name="# of Hairy Vetch flowers per transect") +
  theme_bw(base_size=12)

vetch.flowers.2019.ggplot 










## Vetch Seeds 19 ####
x <- as.numeric(time2019.dat$Sampling.Period)
y <- time2019.dat$Vetch.Average.Seed.Pods

vetch.seeds.2019 <- gam(y ~ s(x), method = "REML")
summary(vetch.seeds.2019)

# ok so this is NOT a good model
resid(vetch.seeds.2019) %>% Box.test()


# make model predictions the same length as the original dataset (so you can map on raw data/dates)
model_p <- predict_gam(vetch.seeds.2019, length_out = 258)

# make the model output in ggplot
pgam_plot <- model_p %>%
  ggplot(aes(x, fit)) +
  geom_smooth_ci(ci_alpha=.3)


# plots points and labels for the correct original data
vetch.pods.2019.ggplot <- pgam_plot +
  geom_point(aes(y=time2019.dat$Vetch.Average.Seed.Pods)) +
  scale_x_continuous(name="2019 Sampling Periods", breaks = c(0,5,10,15,20)) +
  scale_y_continuous(name="# of Vetch seed pods per transect)") +
  theme_bw(base_size=12)

vetch.pods.2019.ggplot 







## P Size 19 ####
x <- as.numeric(time2019.dat$Sampling.Period)
y <- time2019.dat$Pea.Average.Height

pea.height.2019 <- gam(y ~ s(x), method = "REML")
summary(pea.height.2019)


# ok so this is a good model
resid(pea.height.2019) %>% Box.test()

# make model predictions the same length as the original dataset (so you can map on raw data/dates)
model_p <- predict_gam(pea.height.2019, length_out = 258)

# make the model output in ggplot
pgam_plot <- model_p %>%
  ggplot(aes(x, fit)) +
  geom_smooth_ci(ci_alpha=.3)


# plots points and labels for the correct original data
pea.height.2019.ggplot <- pgam_plot +
  geom_point(aes(y=time2019.dat$Pea.Average.Height)) +
  scale_x_continuous(name="2019 Sampling Periods", breaks = c(0,5,10,15,20)) +
  scale_y_continuous(name="Pea growth (average height in cm)") +
  theme_bw(base_size=12)

pea.height.2019.ggplot 







## P Flowers 19 ####
x <- as.numeric(time2019.dat$Sampling.Period)
y <- time2019.dat$Pea.Average.Flowers

pea.flowers.2019 <- gam(y ~ s(x), method = "REML")

#statistics for GAM
summary(pea.flowers.2019)

# ok so this is a good model too
resid(pea.flowers.2019) %>% Box.test()

# make model predictions the same length as the original dataset (so you can map on raw data/dates)
model_p <- predict_gam(pea.flowers.2019, length_out = 258)

# make the model output in ggplot
pgam_plot <- model_p %>%
  ggplot(aes(x, fit)) +
  geom_smooth_ci(ci_alpha=.3)


# plots points and labels for the correct original data
pea.flower.2019.ggplot <- pgam_plot +
  geom_point(aes(y=time2019.dat$Pea.Average.Flowers)) +
  scale_x_continuous(name="2019 Sampling Periods", breaks = c(0,5,10,15,20)) +
  scale_y_continuous(name="# of Pea Flowers Per Transect") +
  theme_bw(base_size=12)

pea.flower.2019.ggplot 








## P Seeds 19 ####
x <- as.numeric(time2019.dat$Sampling.Period)
y <- time2019.dat$Pea.Average.Seed.Pods

pea.seeds.2019 <- gam(y ~ s(x), method = "REML")

summary(pea.seeds.2019)

# ok so this is NOT a good model
resid(pea.seeds.2019) %>% Box.test()

### Points #####
plot(predict.gam(pea.seeds.2019))

predict.gam(pea.seeds.2019)
plot.gam(pea.seeds.2019)

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

gam_minmax(pea.seeds.2019)


### GAM GGPLOT ####
# requires tidymv support for GAMs

# how long is the dataframe?
length(time2019.dat$Pea.Average.Seed.Pods)
# should be 258 obs

# make model predictions the same length as the original dataset (so you can map on raw data/dates)
model_p <- predict_gam(pea.seeds.2019, length_out = 258)

pgam_plot <- model_p %>%
  ggplot(aes(x, fit)) +
  geom_smooth_ci(ci_alpha=.3)

# make a vector for the collection dates to use the in axis ticks
# quickdates_2019 <- c("May 5", "May 22", "June 8","June 25", "July 10")

# currently not used but this can  be backup

# plots points (ugly, but its true)
pea.pod.2019.ggplot <- pgam_plot +
  geom_point(aes(y=time2019.dat$Pea.Average.Seed.Pods)) +
  scale_x_continuous(name="2019 Sampling Periods", breaks = c(0,5,10,15,20)) +
  scale_y_continuous(name="# of Seed Pods Per Transect") +
  theme_bw(base_size=12)
pea.pod.2019.ggplot 


### 2019 ggarrange ######

# arrange into one large panel using ggpubr package
# it should be coverage/height, flowers, pods to show the phenology of peas and vetch
# one column for pea, one for vetch, 3 rows

figure.2019.all <- ggarrange(vetch.cover.2019.ggplot, vetch.flowers.2019.ggplot, vetch.pods.2019.ggplot,
                             pea.height.2019.ggplot, pea.flower.2019.ggplot, pea.pod.2019.ggplot, 
                          labels = c("", "", "", ""), ncol = 3, nrow = 2)
figure.2019.all




# I forget what this is for, probably just delete later:
# ggplot(tips2, aes(x = reorder(day, -perc), y = perc)) + geom_bar(stat = "identity")






# 2018 #####
# moved to paper 1 ####
legume.2018.dat <- read.xlsx("legume data 2018.xlsx", 1, header=TRUE) %>% filter(Crop.type == "Non-crop")

# convert abundance to density
legume.2018.dat$Total.Aphid.Abudance <- log((legume.2018.dat$Total.Aphid.Abudance/legume.2018.dat$Total.Plant.Coverage)+1)
str(legume.2018.dat)

# Fig 2 ####
#aphid density fig for 2018 non-crop legume transects
aphid.hosts.fig <- ggplot(legume.2018.dat, aes(x=Total.Aphid.Abudance, 
                                               y=reorder(Plant.Species,-desc(Total.Aphid.Abudance)),
                                               fill=PEMV.Presence)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Legume species from 10m transects", x="Aphid density (Log aphid # per meter)", fill="PEMV Present") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(axis.text.y = element_text(face = "italic")) +
  theme(legend.position=c(0.8,0.2))
aphid.hosts.fig

# write figure 2 to folder, use arguments to modify size or file type!
ggsave(filename = "pea value fig 2.svg", plot = aphid.hosts.fig, device = "svg",
       width = 6, height = 5, units = "in")

#### Pilot GPS Fig ####
library(ggplot2)
library(ggmap)
library(emmeans)
devtools::install_github("dkahle/ggmap")
trapLocation <- c(-117.6300,46.3799,-116.8988,47.124)
## then use the 'get_map' command to grab rasters from the internet to make your map
trap.Map <- get_map(location=trapLocation,
                    source="stamen", maptype="terrain-background", crop=FALSE)


## myLocation <- c(left long., bottom lat., right long., upper lat.)

gps.map <- get_map(location=trapLocation, source="stamen", maptype="terrain", crop=FALSE)
ggmap(gps.map)

sites.map <- ggmap(gps.map)+ geom_point(data=dat, aes(x=long, y=lat), size=4, color="red") + labs(x="Latitude",y= "Longitude")
sites.map


##view the map you just made!

ggmap(trap.Map)

dat <- read.csv("2018 GPS locations and site data.csv", header = TRUE, sep = ",")
str(dat)

plant.map <- ggmap(trap.Map) + geom_point(data=dat,aes(x=long,y=lat,color=Site.Dominant), size=4) +
  theme(legend.title=element_blank()) + labs(x="Latitude",y= "Longitude")

plant.map

# Plant.Map is just a map of the dominant legume at each survey location


ptmap <- ggmap(trap.Map) + geom_point(data=dat, aes(x=long, y=lat, size=aphid.count, shape=virus, fill=host.plant), size=4.2, alpha=0.5)
ptmap

#pilot map with psuedo data (not complete)
# this map will show aphid count and whether we found virus in 2018

pilot <- read.csv("2018 GPS locations and site data.csv", header=TRUE, sep=",")
pilot

pilot.map <- ggmap(trap.Map) + geom_point(data=pilot, aes(x=long, y=lat, size=Aphid.count, color=Virus.at.site), alpha=0.5) +
  scale_color_manual(values=c("blue", "red")) +
  labs(x="Latitude",y= "Longitude", size = "Aphid Count (# per sweep)", color="PEMV Present") +
  scale_size_continuous(range = c(4, 20)) +
  guides(colour = guide_legend(override.aes = list(size=10)))

pilot.map


# pilot density maps for wheat talk tomorrow

density.map <- ggmap(trap.Map) +
  stat_density2d(aes(x = long, y = lat, fill = ..level..),
                 size = 0.5, data = pilot %>% filter(Site.Dominant %in%
                                                       c("Pea", "Hairy Vetch")),
                 alpha = 0.25,
                 geom = "polygon") +
  scale_fill_distiller(palette = "PuOr")
density.map

pea.density.map <- ggmap(trap.Map) +
  stat_density2d(aes(x = long, y = lat, fill = ..level..),
                 size = 0.5, data = pilot %>% filter(Site.Dominant == "Pea"),
                 alpha = 0.25,
                 geom = "polygon") +
  scale_fill_distiller(palette = "PuOr")

pea.density.map





#scale aphid count 1 - 0 (probably not reliable)
library("scales")
pilot$aphid.scale <- rescale(pilot$Aphid.count, to=c(0,1))
pilot$aphid.scale

# pilot density map #

density.map <- ggmap(trap.Map) +
  stat_density2d(aes(x = long, y = lat, fill = aphid.scale, alpha = 1),
                 size = 0.5, bins = 4, data = pilot,
                 geom = "polygon")

density.map


#pilot interpolation #
interp.dat <- gstat::idw(formula = new_type ~ 1, locations = mb_spat_WGS84, newdata = grd)

interp.dat = as.data.frame(interp.dat) 

density.map <- ggmap(trap.Map) +
  #stat_density2d(aes(x = long, y = lat, fill = Aphid.count),
  #              size = 1, bins = 20, data = pilot,
  #             geom = "polygon") +
  geom_tile(data =pilot, aes(x = long, y = lat, fill = Aphid.count)) +
  scale_fill_distiller(palette = "PuOr", direction = 1)
density.map

# texas example inverse distance weighting #
library(rgdal)
library(tmap)

# Load precipitation data
z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/precip.rds"))
P <- readRDS(z)

# Load Texas boudary map
z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
W <- readRDS(z)

# Replace point boundary extent with that of Texas
P@bbox <- W@bbox

###  IDW 
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
library(raster)

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Precip_in ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
library("raster")
r       <- raster(P.idw)
r.m     <- mask(r, W)

# Plot
library("tmap")

tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

############### Rylee 2018 Figs ################

rylee.dat <- read.csv("2018 Rylee Native Plant Pilot.csv", header = TRUE)
rylee.dat

rylee.dat$Duration <- as.factor(rylee.dat$Duration)

rylee.mod <- glm.nb(Aphid.Count ~ Host.Plant*Duration, data=rylee.dat)
summary(rylee.mod)
Anova(rylee.mod)

duration.source.lsm <- cld(emmeans(rylee.mod, ~Host.Plant*Duration, type="response"))
duration.source.lsm 

duration.source.plot <- ggplot(data=duration.source.lsm, aes(x = Duration, y = response, shape = Host.Plant)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=response-SE, ymax=response+SE), width=0, size=1) +
  geom_line(aes(group=Host.Plant), size=1) +
  theme(legend.position=c(0.7,0.9)) +
  ylab("Aphids per plant") +
  xlab("Duration (days)")
duration.source.plot

#day.eight.plot

rylee.eight <- subset(rylee.dat, Duration == "8")
rylee.eight

eight.mod <- glm.nb(Aphid.Count ~ Host.Plant, data=rylee.eight)
summary(eight.mod)
Anova(eight.mod)

eight.cld <- cld(emmeans(eight.mod, ~ Host.Plant, type="response"))
eight.cld

eight.plot <- ggplot(data=eight.cld, aes(x = Host.Plant, y = response)) +
  geom_bar(stat="identity", width=0.5) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE)), position=position_dodge(0.5), width=0.2) +
  theme_bw(base_size = 12) + 
  labs(y="Mean Aphid Abundance after 8 days", x = "Host-Plant Species")

eight.plot

tall.mod <- glm(delta.height ~ Host.Plant, data=rylee.dat)
Anova(tall.mod)

tall.cld <- cld(emmeans(tall.mod, ~Host.Plant, typpe="response"))
tall.cld

tall.plot <- ggplot(data=tall.cld, aes(x=Host.Plant, y = emmean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  labs(y="Change in plant height after 8 days (cm)", x = "Host-Plant Species")
tall.plot

# Ten-year data #####

ten.dat <- read.csv("Aphid.Records2.csv")
str(ten.dat)

ts(log(ten.dat$AphidCount+1)) %>% plot()

# make a simple model 

aphid.glm <- glm.nb(AphidCount ~ as.factor(Year)*as.factor(Julian_week), data=ten.dat)
summary(aphid.glm)

emmeans(aphid.glm, ~Julian_week*Year)
