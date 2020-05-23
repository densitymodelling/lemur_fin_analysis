# some plots for presentation

load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")
load("RData/4_process_uncertainty.RData")

library(ggplot2)
library(mapdata)
library(tidyr)
library(dplyr)

# get USA map for plots
w <- map_data("worldHires", ylim = range(cce_poly$y), xlim = range(cce_poly$x))


# plot mean predictions
summary_predgrid_all$avv_d <- cut(summary_predgrid_all$avv, c(0,0.001,.0023,.0036,.0085,.036,1))
# remove NAs
summary_predgrid_all <- summary_predgrid_all[!is.na(summary_predgrid_all$avv_d), ]

p_pred <- ggplot(summary_predgrid_all, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=avv_d)) +
  geom_point(aes(y=mlat, x=mlon), data=subset(fin, count>0),
             shape = 21, colour = "black", fill = "white", size=0.75) +
  scale_fill_viridis_d() +
  labs(x="", y="", fill="Density") +
  coord_map(ylim=range(summary_predgrid_all$mlat), xlim=range(summary_predgrid_all$mlon)) +
  theme_minimal()
#print(p_pred)

ggsave(p_pred, file="figures/pred_d.pdf", width=7, height=9)


# standard deviations
summary_predgrid_all$sdd_d <- cut(summary_predgrid_all$sdd, c(0,0.001,.0023,.0036,.0085,.036,1, 2))

p_sd <- ggplot(summary_predgrid_all, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=sdd_d)) +
  geom_point(aes(y=mlat, x=mlon), data=subset(fin, count>0),
             shape = 21, colour = "black", fill = "white", size=0.75) +
  scale_fill_viridis_d() +
  labs(x="", y="", fill="sd") +
  coord_map(ylim=range(summary_predgrid_all$mlat), xlim=range(summary_predgrid_all$mlon)) +
  theme_minimal()
#print(p_sd)

ggsave(p_sd, file="figures/sd_d.pdf", width=7, height=9)

# standard deviations with g0
summary_predgrid_all$sdd_g0_d <- cut(summary_predgrid_all$sdd_g0, c(0,0.001,.0023,.0036,.0085,.036,1, 2))

p_sd_g0 <- ggplot(summary_predgrid_all, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=sdd_g0_d)) +
  geom_point(aes(y=mlat, x=mlon), data=subset(fin, count>0),
             shape = 21, colour = "black", fill = "white", size=0.75) +
  scale_fill_viridis_d() +
  labs(x="", y="", fill="sd") +
  coord_map(ylim=range(summary_predgrid_all$mlat), xlim=range(summary_predgrid_all$mlon)) +
  theme_minimal()
#print(p_sd_g0)

ggsave(p_sd_g0, file="figures/sd_d_g0.pdf", width=7, height=9)


## coefficients of variation
#summary_predgrid_all$cvd_d <- summary_predgrid_all$sdd/summary_predgrid_all$avv
#
#p_cv <- ggplot(summary_predgrid_all, aes(y=mlat, x=mlon)) +
#  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#  geom_tile(aes(fill=cvd_d)) +
#  scale_fill_viridis_c() +
#  labs(x="", y="", fill="cv") +
#  coord_map(ylim=range(summary_predgrid_all$mlat), xlim=range(summary_predgrid_all$mlon)) +
#  theme_minimal()
#print(p_cv)



# plot yearly predictions and uncertainties
summary_predgrid_yearly$avv_d <- cut(summary_predgrid_yearly$avv, c(0,0.001,.0023,.0036,.0085,.036,1))
# remove NAs
summary_predgrid_yearly <- summary_predgrid_yearly[!is.na(summary_predgrid_yearly$avv_d), ]
# standard deviations
summary_predgrid_yearly$sdd_d <- cut(summary_predgrid_yearly$sdd, c(0,0.001,.0023,.0036,.0085,.036,1, 2))

# make the data long rather than wide
summary_predgrid_yearly <- summary_predgrid_yearly %>%
  # only need these columns
  select(mlon, mlat, year, avv_d, sdd_d) %>%
  # make a long rather than wide data.frame
  pivot_longer(c(avv_d, sdd_d))

# name the data types correctly
summary_predgrid_yearly$name <- factor(summary_predgrid_yearly$name, labels=c("Density","SD"))  

# make the plot
p_yearly <- ggplot(summary_predgrid_yearly, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=value)) +
  geom_point(aes(y=mlat, x=mlon), data=subset(fin, count>0),
             shape = 21, colour = "black", fill = "white", size=0.75) +
  scale_fill_viridis_d() +
  facet_grid(vars(name), vars(year)) +
  labs(x="", y="", fill="Value") +
  coord_map(ylim=range(summary_predgrid_yearly$mlat), xlim=range(summary_predgrid_yearly$mlon)) +
  theme_minimal()
#print(p_yearly)

ggsave(p_yearly, file="figures/yearlies.pdf", width=7, height=9)


