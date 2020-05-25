# investigate 2008 high uncertainty

Nhat <- read.csv("out/Nhat_ests.csv")
Nhat$X <- NULL
out_pred_files <- dir("out", pattern="_pred.csv", full.names = TRUE)
dates <- sub("out/CCE_0.1deg_", "", out_pred_files)
dates <- sub("_pred.csv","",dates)
library(lubridate)
dates <- ymd(dates)
plot_Nhat <- data.frame(means = apply(Nhat,1, median, na.rm=TRUE),
                        upper = apply(Nhat,1, quantile, probs=0.975, na.rm=TRUE),
                        lower = apply(Nhat,1, quantile, probs=0.025, na.rm=TRUE),
                        date  = dates)

# where is the max?
up <- which.max(plot_Nhat$upper)
plot_Nhat[up,]
#        means    upper    lower       date
# 165 6773.068 20251.32 2903.808 2008-06-26

hist(unlist(Nhat[dates==ymd("2008-06-26"),]), breaks=100,
     xlab="Abundance")
mean(unlist(Nhat[dates==ymd("2008-06-26"),]))
median(unlist(Nhat[dates==ymd("2008-06-26"),]))

library(ggplot2)
library(mapdata)



pred_file <- "data/roms_grids/CCE_0.1deg_2008-06-26.csv"
source("support_scripts/prepare_preds.R")
load("RData/0_format_aux_data.RData")

# get USA map for plots
w <- map_data("worldHires", ylim = range(cce_poly$y), xlim = range(cce_poly$x))

summary_predgrid <- prepare_preds(pred_file, pred_areas=pred_areas, poly=cce_poly)

# read-in and calculate the appropriate summaries
out_pred_file <- "out/CCE_0.1deg_2008-06-26_pred.csv"
this_pred <- data.table::fread(out_pred_file, header=TRUE, sep=",")

this_pred <- as.data.frame(this_pred)
summary_predgrid$avv <- apply(this_pred, 1, mean)

# plot mean predictions
summary_predgrid$avv_d <- cut(summary_predgrid$avv, c(0,0.001,.0023,.0036,.0085,.036,1))
# remove NAs
summary_predgrid <- summary_predgrid[!is.na(summary_predgrid$avv_d), ]

p_pred <- ggplot(summary_predgrid, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=avv_d)) +
  #geom_point(aes(y=mlat, x=mlon), data=subset(fin, count>0),
  #           shape = 21, colour = "black", fill = "white", size=0.75) +
  scale_fill_viridis_d() +
  labs(x="", y="", fill="Density") +
  coord_map(ylim=range(summary_predgrid$mlat), xlim=range(summary_predgrid$mlon)) +
  theme_minimal()

library(dsm)
load("RData/2_prop_that_var.RData")
summary_predgrid$XX <- matrix(0, nrow=nrow(summary_predgrid), ncol=18)
predplot <- dsm::plot_pred_by_term(b_vp, summary_predgrid,
                                   c("mlon", "mlat")) +
  scale_fill_viridis_c() +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  coord_map(ylim=range(summary_predgrid$mlat), xlim=range(summary_predgrid$mlon)) +
  labs(x="", y="") +
  theme_minimal()
print(predplot)

ggsave(predplot, file="figures/lpplot-2008-06-26.pdf", width=10, height=10)

p_ild <- ggplot(summary_predgrid, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=ild)) +
  scale_fill_viridis_c() +
  labs(x="", y="") +
  coord_map(ylim=range(summary_predgrid$mlat), xlim=range(summary_predgrid$mlon)) +
  theme_minimal()

library(patchwork)

p_pred + p_ild

