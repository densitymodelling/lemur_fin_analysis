library(mgcv)

load("RData/2_prop_that_var.RData")

pdf("figures/gamplot.pdf", width=10, height=8)

plot(b_vp, pages=1, scheme=2, shade=TRUE)

dev.off()
