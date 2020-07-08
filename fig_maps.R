# plot maps of density and uncertainty

load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")
load("RData/4_process_uncertainty.RData")

library(ggplot2)
library(mapdata)
library(tidyr)
library(dplyr)

# get USA map for plots
w <- map_data("worldHires", ylim = range(cce_poly$y), xlim = range(cce_poly$x))

colour_cats <- c(0,0.001,.0023,.0036,.0085,.036,1)

# plot mean predictions
#summary_predgrid_all$avv_d <- cut(summary_predgrid_all$avv, colour_cats)
# remove NAs
#summary_predgrid_all <- summary_predgrid_all[!is.na(summary_predgrid_all$avv_d), ]

p_pred <- ggplot(summary_predgrid_all, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  #geom_tile(aes(fill=avv_d)) +
  geom_contour_filled(aes(z=avv), breaks=colour_cats,
                      show.legend = TRUE) +
  geom_point(aes(y=mlat, x=mlon), data=subset(fin, count>0),
             shape = 21, colour = "black", fill = "white", size=0.75) +
  scale_fill_viridis_d(drop=FALSE) +
  labs(x="", y="", fill="Density") +
  coord_map(ylim=range(summary_predgrid_all$mlat),
            xlim=range(summary_predgrid_all$mlon)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.text = element_text(size=12))
print(p_pred)

ggsave(p_pred, file="figures/pred_d.pdf", width=7, height=9)


## standard deviations
#summary_predgrid_all$sdd_d <- cut(summary_predgrid_all$sdd, colour_cats)
#
#p_sd <- ggplot(summary_predgrid_all, aes(y=mlat, x=mlon)) +
#  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#  geom_tile(aes(fill=sdd_d)) +
#  geom_point(aes(y=mlat, x=mlon), data=subset(fin, count>0),
#             shape = 21, colour = "black", fill = "white", size=0.75) +
#  scale_fill_viridis_d() +
#  labs(x="", y="", fill="Standard\nerror") +
#  coord_map(ylim=range(summary_predgrid_all$mlat), xlim=range(summary_predgrid_all$mlon)) +
#  theme_minimal()
##print(p_sd)

#ggsave(p_sd, file="figures/unc_d.pdf", width=7, height=9)

# standard deviations with g0
#summary_predgrid_all$sdd_g0_d <- cut(summary_predgrid_all$sdd_g0, colour_cats)

p_sd_g0 <- ggplot(summary_predgrid_all, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  #geom_tile(aes(fill=sdd_g0_d)) +
  geom_contour_filled(aes(z=sdd_g0), breaks=colour_cats) +
  geom_point(aes(y=mlat, x=mlon), data=subset(fin, count>0),
             shape = 21, colour = "black", fill = "white", size=0.75) +
  scale_colour_viridis_d(drop=FALSE) +
#  scale_fill_viridis_d(limits=range(colour_cats), 
#                       position="bottom") +
  labs(x="", y="", fill="Standard\nerror") +
  coord_map(ylim=range(summary_predgrid_all$mlat), xlim=range(summary_predgrid_all$mlon)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.text = element_text(size=12))
print(p_sd_g0)

ggsave(p_sd_g0, file="figures/unc_d_g0.pdf", width=7, height=9)


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
#summary_predgrid_yearly$avv_d <- cut(summary_predgrid_yearly$avv, c(0,0.001,.0023,.0036,.0085,.036,1))
# remove NAs
#summary_predgrid_yearly <- summary_predgrid_yearly[!is.na(summary_predgrid_yearly$avv_d), ]
# standard deviations
#summary_predgrid_yearly$sdd_d <- cut(summary_predgrid_yearly$sdd, c(0,0.001,.0023,.0036,.0085,.036,1, 2))

# make the data long rather than wide
summary_predgrid_yearly <- summary_predgrid_yearly %>%
  # only need these columns
  #select(mlon, mlat, year, avv_d, sdd_d) %>%
  select(mlon, mlat, year, avv, sdd_g0) %>%
  # make a long rather than wide data.frame
  #pivot_longer(c(avv_d, sdd_d))
  pivot_longer(c(avv, sdd_g0))

# name the data types correctly
summary_predgrid_yearly$name <- factor(summary_predgrid_yearly$name,
                                       labels=c("Density", "Standard error"))  

# make the plot
p_yearly <- ggplot(summary_predgrid_yearly, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  #geom_tile(aes(fill=value)) +
  geom_contour_filled(aes(z=value), breaks=colour_cats) +
  geom_point(aes(y=mlat, x=mlon), data=subset(fin, count>0),
             shape = 21, colour = "black", fill = "white", size=0.75) +
  scale_fill_viridis_d() +
  facet_grid(vars(name), vars(year)) +
  labs(x="", y="", fill="Value") +
  coord_map(ylim=range(summary_predgrid_yearly$mlat), xlim=range(summary_predgrid_yearly$mlon)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.text = element_text(size=12))
print(p_yearly)

ggsave(p_yearly, file="figures/yearlies.pdf", width=10, height=9)


