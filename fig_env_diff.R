## plot differences in maps of density and uncertainty
library(ggplot2)
library(mapdata)
library(tidyr)
library(dplyr)

# load all data
load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")
load("RData/4_process_uncertainty.RData")

# for full uncertainty model, rename appropriately
summary_predgrid_all_all <- summary_predgrid_all
summary_predgrid_yearly_all <- summary_predgrid_yearly

# for the environmental uncertainty only model
load("RData/env_only_4_process_uncertainty.RData")
summary_predgrid_env_only <- summary_predgrid_all
summary_predgrid_yearly_env_only <- summary_predgrid_yearly

# make a new "differences" data.frame
summary_predgrid_diff <- summary_predgrid_all
summary_predgrid_yearly_diff <- summary_predgrid_yearly

# calculate standard error differences
summary_predgrid_diff$sediff <- summary_predgrid_all_all$sdd_g0 -
                                summary_predgrid_env_only$sdd
summary_predgrid_diff <- summary_predgrid_diff[!is.na(summary_predgrid_diff$sediff), ]


## yearlies

summary_predgrid_yearly_diff$avvdiff <- summary_predgrid_yearly_all$avv -
                                summary_predgrid_yearly_env_only$avv
summary_predgrid_yearly_diff$sediff <- summary_predgrid_yearly_all$sdd_g0 -
                                summary_predgrid_yearly_env_only$sdd
summary_predgrid_yearly_diff <- summary_predgrid_yearly_diff[!is.na(summary_predgrid_yearly_diff$sediff), ]

# get USA map for plots
w <- map_data("worldHires", ylim = range(cce_poly$y), xlim = range(cce_poly$x))



## differences standard deviations
p_sd_diff <- ggplot(summary_predgrid_diff, aes(y=mlat, x=mlon)) +
  geom_polygon(data=w, aes(x=long, y=lat, group=group), fill="grey80")+
  geom_tile(aes(fill=sediff)) +
#  scale_fill_viridis_c(trans="log", breaks=c(1e-4, 0.05, 0.14), option="A") +
  scale_fill_gradient2(limits=c(0, 0.145)) +
  labs(x="", y="", fill="Difference in\nstandard\nerror") +
  coord_map(ylim=range(summary_predgrid_all$mlat),
            xlim=range(summary_predgrid_all$mlon)) +
  #coord_equal(ylim=range(summary_predgrid_all$mlat),
  #          xlim=range(summary_predgrid_all$mlon)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.text = element_text(size=12))
#print(p_sd_diff)

ggsave(p_sd_diff, file="figures/diff_unc_d_g0.pdf", width=7, height=9)

p_yearly_sd_diff <- ggplot(summary_predgrid_yearly_diff, aes(y=mlat, x=mlon)) +
  geom_polygon(data=w, aes(x=long, y=lat, group=group), fill="grey80")+
  geom_tile(aes(fill=sediff)) +
  scale_fill_gradient2(limits=c(0, 0.323)) +
  #scale_fill_viridis_c(trans="log", breaks=c(1e-4, 0.05, 0.14), option="A") +
  facet_grid(~year) +
  labs(x="", y="", fill="Difference in\nstandard\nerror") +
  #coord_equal(ylim=range(summary_predgrid_all$mlat),
  #          xlim=range(summary_predgrid_all$mlon)) +
  coord_map(ylim=range(summary_predgrid_all$mlat),
            xlim=range(summary_predgrid_all$mlon)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.text = element_text(size=12))
#print(p_yearly_sd_diff)

ggsave(p_yearly_sd_diff, file="figures/yearly_diff_unc_d.pdf", width=7, height=4)



