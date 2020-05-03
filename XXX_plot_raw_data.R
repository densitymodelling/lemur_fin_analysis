# download depth data from ERDDAP for publication

library(ggplot2)
library(mapdata)
library(cmocean)



depthdat <- read.csv("data/Depth_ocean_CCEgrid.csv")

load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")

x <- depthdat$lon180
y <- depthdat$lat

library(mgcv)

ind <- inSide(cce_poly, x, y)

depthdat <- depthdat[ind,]

pal <- cmocean('deep')(256)

w <- map_data("worldHires", ylim = range(cce_poly$y), xlim = range(cce_poly$x))

p_dat <- ggplot() +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(data=depthdat, aes(x = lon180, y = lat, fill = Depth_ETOPO1),
              interpolate = FALSE) +
  scale_fill_gradientn(colours=pal) +
  geom_point(aes(y=mlat, x=mlon), size=0.2, data=fin) +
  geom_point(aes(y=mlat, x=mlon), colour="#edf8b1", size=0.3, data=subset(fin, count>0)) +
  theme_minimal() +
  labs(x="", y="", fill="Depth") +
  coord_fixed(1.3, ylim = range(cce_poly$y), xlim = range(cce_poly$x))
print(p_dat)

ggsave(p_dat, file="figures/rawdat.pdf", width=5, height=7)


