# fin example

# this first file gets the data in order and fits a spatial model to it
# data provided by Elizabeth Becker, SWFSC unless otherwise noted

set.seed(42)

# load the segment data
fin <- read.csv("data/Bp_ModelingSegs_BF0_5_CCE_1996_2014_no09_5km.csv")
# load the detection function, provided by Jay Barlow, SWFSC
load("data/ddf Object LgBaleenWh_1991-2018_BEAUF 0-6.RData")


# in the data, the following columns are interesting:
# InCCE      - same everywhere (in Cali current)
# efftyp     - S(tandard), N(onstandard), F(ine lines). F excluded.
# mtime      - H.M*60
# dist       - Effort
# aveBF      - beaufort
# nSI.74     - number of *sightings* of fins
# ANI.74     - total number of animals
# ild        - mixed layer depth
# ssh        - sea suface height
# sshsd      - standard deviation of sea suface height
# sst        - sea surface temperature
# sstsd      - standard deviation of sea surface temperature
# depth      - bathymetric depth
# mlat, mlon - lat and long

# load libraries
library(mgcv)
library(Distance)
library(dsm)
source("support_scripts/obs_exp.R")


# get the seg data in order
# discard the fine lines
#fin <- subset(fin, efftyp!="F")

# get rid of any data row with NAs
# drop 9 segments without ROMS data
na_ind <- apply(fin, 1, function(x) any(is.na(x)))
fin <- fin[!na_ind,]

# get the data to match ships IDs to cruise numbers
ships <- read.csv("data/Ships.CSV")
names(ships) <- c("cruiseNum", "Ship")
fin <- merge(fin, ships)
fin$Ship <- factor(as.character(fin$Ship),
                   levels=levels(LTfitBest$data$Ship),
                   labels=levels(LTfitBest$data$Ship))

# load average group sizes
sizes <- read.csv("data/LgWhale_CCE_1991_2014_GS.csv")

# make the prediction grid for the detection probabilities
# > LTfitBest$ds$aux$ddfobj$scale$formula
# [1] "~1 + Beauf + Ship + SppMax + Vis + LnTotSS"
fin$Beauf <- fin$aveBF
fin$LnTotSS <- log(sizes$AveGS[sizes$SpCode==74])
fin$SppMax <- factor("074", levels=levels(LTfitBest$data$SppMax))
fin$Vis <- fin$aveVis
fin$Vis[fin$Vis>6] <- 6

# dummy column for predict() to work
fin$distance <- 0
# get the ps
fin$p <- predict(LTfitBest, newdata=fin, compute=TRUE)$fitted
# remove dummy again
fin$distance <- NULL

# rename "Effort" column (length of segment
fin$Effort <- fin$dist
fin$dist <- NULL

# extract the truncation
width <- LTfitBest$meta.data$width


# from Barlow and Forney (2007)
g0 <- 0.921
g0_CV <- 0.023

# calculate the offset
fin$off <- log(fin$p * g0 * 2 * width * fin$Effort)

# some additional data cleaning here to make things easier later
# rename the response
fin$count <- fin$ANI.74
fin$ANI.74 <- NULL

# remove unneeded columns
fin$cruiseNum <- fin$efftyp <- fin$aveBF <-
fin$Idnum <- fin$InCCE <- fin$aveSwell <-
fin$aveVis <- NULL
# dsm only knows about what to do with p not ESW
#fin$ESW.74 <- NULL

# use Sample.Label
fin$Sample.Label <- fin$segnum
fin$segnum <- NULL


# fit a model
maxdf <- 10
spline2use <- "ts"
b <- gam(count ~ offset(off) +
                s(sst, bs = spline2use, k = maxdf) + 
                s(sst_sd, bs = spline2use, k = maxdf) +
                s(ssh, bs = spline2use, k = maxdf) +
                s(ild, bs = spline2use, k = maxdf) +
                s(year, bs = spline2use, k = 5) +
                te(mlon, mlat, bs = spline2use, k = 6),
         family=tw(), method="REML", data=fin,
         control=list(keepData=TRUE))
# having keepData here is essential for varprop!

summary(b)

# attach the detection function into the gam object
b$ddf <- LTfitBest
# make it a dsm object
class(b) <- c("dsm", class(b))


# how does the model do vs observed...
# ... at different Beauforts
# check once correct df used!
obs_exp(b, "Beauf", c(0, 1, 2, 3, 4, 5))

gam.check(b, rep=200)
# look into QQ plot stuff?


dsm::rqgam.check(b)

# this model isn't perfect, but it'll do for illustration!


# save what we need later
save(b, fin, g0, g0_CV, file="RData/1_model_and_data.RData")
