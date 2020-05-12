# some plots for presentation

load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")
load("RData/4_process_uncertainty.RData")

library(ggplot2)
library(mapdata)


# get USA map for plots
w <- map_data("worldHires", ylim = range(cce_poly$y), xlim = range(cce_poly$x))


# plot mean predictions
summary_predgrid$avv_d <- cut(summary_predgrid$avv, c(0,0.001,.0023,.0036,.0085,.036,1))
# remove NAs
summary_predgrid <- summary_predgrid[!is.na(summary_predgrid$avv_d), ]

p_pred <- ggplot(summary_predgrid, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=avv_d)) +
  scale_fill_viridis_d() +
  labs(x="", y="", fill="Density") +
  coord_map(ylim=range(summary_predgrid$mlat), xlim=range(summary_predgrid$mlon)) +
  theme_minimal()
print(p_pred)

ggsave(p_pred, file="figures/pred_d.pdf", width=7, height=9)


# standard deviations
summary_predgrid$sdd_d <- cut(summary_predgrid$sdd, c(0,0.001,.0023,.0036,.0085,.036,1, 2))

p_sd <- ggplot(summary_predgrid, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=sdd_d)) +
  scale_fill_viridis_d() +
  labs(x="", y="", fill="sd") +
  coord_map(ylim=range(summary_predgrid$mlat), xlim=range(summary_predgrid$mlon)) +
  theme_minimal()
print(p_sd)

ggsave(p_sd, file="figures/sd_d.pdf", width=7, height=9)

# standard deviations with g0
summary_predgrid$sdd_g0_d <- cut(summary_predgrid$sdd_g0, c(0,0.001,.0023,.0036,.0085,.036,1, 2))

p_sd_g0 <- ggplot(summary_predgrid, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=sdd_g0_d)) +
  scale_fill_viridis_d() +
  labs(x="", y="", fill="sd") +
  coord_map(ylim=range(summary_predgrid$mlat), xlim=range(summary_predgrid$mlon)) +
  theme_minimal()
print(p_sd_g0)

ggsave(p_sd_g0, file="figures/sd_d_g0.pdf", width=7, height=9)


# coefficients of variation
summary_predgrid$cvd_d <- summary_predgrid$sdd/summary_predgrid$avv

p_cv <- ggplot(summary_predgrid, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=cvd_d)) +
  scale_fill_viridis_c() +
  labs(x="", y="", fill="cv") +
  coord_map(ylim=range(summary_predgrid$mlat), xlim=range(summary_predgrid$mlon)) +
  theme_minimal()
print(p_cv)


# Nhat time series
# need to fiddle with dates to get the data nicely plotable
Nhat <- read.csv("out/Nhat_ests.csv")
Nhat_fix <- boxplot.stats(as.matrix(Nhat))
Nhat2 <- Nhat
Nhat2[Nhat2>Nhat_fix$stats[5]] <- NA
out_pred_files <- dir("out", pattern="_pred.csv", full.names = TRUE)
dates <- sub("out/CCE_0.1deg_", "", out_pred_files)
dates <- sub("_pred.csv","",dates)
library(lubridate)
dates <- ymd(dates)
plot_Nhat <- data.frame(means = apply(Nhat2,1, median, na.rm=TRUE),
                        upper = apply(Nhat2,1, quantile, probs=0.975, na.rm=TRUE),
                        lower = apply(Nhat2,1, quantile, probs=0.025, na.rm=TRUE),
                        date  = dates)

plot_Nhat$year <- year(plot_Nhat$date)

p_Nhat <- ggplot(plot_Nhat, aes(x=date))+
  geom_line(aes(y=means)) +
  geom_line(aes(y=upper), lty=2) +
  geom_line(aes(y=lower), lty=2) +
  facet_wrap(~year, scales = "free_x") +
  labs(x="Date", y="Abundance") +
  theme_minimal()
p_Nhat

ggsave(p_Nhat, file="figures/Nhat.pdf", width=8, height=7)



