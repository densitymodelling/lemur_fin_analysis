# plot transects, observations, bathymetry

library(ggplot2)
library(mapdata)
library(cmocean)
library(mgcv)


load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")

# setup depth data
depthdat <- read.csv("data/Depth_ocean_CCEgrid.csv")
x <- depthdat$lon180
y <- depthdat$lat
ind <- inSide(cce_poly, x, y)
depthdat <- depthdat[ind,]

# make some breaks
depth_breaks <- quantile(depthdat$Depth_ETOPO1, prob=seq(0,1, len=8),na.rm=TRUE)

# setup palette for plotting
pal <- cmocean('deep')(length(depth_breaks)+2)

# map
w <- map_data("worldHires", ylim = range(cce_poly$y), xlim = range(cce_poly$x))

# plot bathymetry with effort and observations on map
p_dat <- ggplot() +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#  geom_tile(data=depthdat, aes(x = lon180, y = lat, fill = Depth_ETOPO1)) +
  geom_contour_filled(data=depthdat, aes(x=lon180, y=lat, z=Depth_ETOPO1),
                      breaks=depth_breaks) +
#  scale_fill_gradientn(colours=pal) +
  scale_fill_discrete(type=pal) +
  geom_point(aes(y=mlat, x=mlon), size=0.2, data=fin) +
  geom_point(aes(y=mlat, x=mlon), colour="#edf8b1", size=0.3, data=subset(fin, count>0)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
#        legend.key.width = unit(4, "inch"),
        axis.text = element_text(size=12)) +
  labs(x="", y="", fill="Depth\n(m)") +
  coord_map(ylim = c(30.2, 48.0), xlim = c(-131.0, -117.3))
#print(p_dat)

ggsave(p_dat, file="figures/rawdat.pdf", width=7, height=9)


# make another plot for figure that has e.g. prediction grid

source("support_scripts/prepare_preds.R")

pp <- prepare_preds("data/CCE_0.1deg_2008-09-13.csv", cce_poly, pred_areas)

# just plot mixed layer depth

p_ild <- ggplot() +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(data=pp, aes(x = mlon, y = mlat, fill = ild))+
  scale_fill_gradientn(colours=pal) +
  theme_minimal() +
  labs(x="", y="", fill="MLD") +
  coord_fixed(1.3, ylim = range(cce_poly$y), xlim = range(cce_poly$x))
print(p_ild)

ggsave(p_ild, file="figures/raster_ex.pdf", width=5, height=7)


