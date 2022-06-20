# plot the smooths in the spatial model

library(mgcv)

# load the fitted model
load("RData/2_prop_that_var.RData")

# prepare graphics device
pdf("figures/gamplot.pdf", width=10, height=8)

#plot(b_vp, pages=1, scheme=2, shade=TRUE)
# setup the plot
par(mfrow=c(2, 3))
plot(b_vp, select=1, rug=TRUE, shade=TRUE, xlab="Sea surface temperature (C)")
plot(b_vp, select=2, rug=TRUE, shade=TRUE, xlab="Standard deviation of SST (C)")
plot(b_vp, select=3, rug=TRUE, shade=TRUE, xlab="Sea surface height (m)")
plot(b_vp, select=4, rug=TRUE, shade=TRUE, xlab="Mixed layer depth")
plot(b_vp, select=5, rug=TRUE, shade=TRUE, xlab="Year")
plot(b_vp, select=6, scheme=2, asp=1, xlab="Longitude", ylab="Latitude",
     main="")

# close device, writing the PDF
dev.off()
