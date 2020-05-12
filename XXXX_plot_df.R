# detection function plots

library(mrds)
library(Distance)
library(RColorBrewer)

#load("RData/2_prop_that_var.RData")
load("RData/1_model_and_data.RData")


fin_ddf <- b$ddf


pdf(file="figures/detection_functions.pdf", width=10, height=12)

par(mfrow=c(2,2))


# by ship
plot(fin_ddf, showpoints=FALSE)
plot_df <- expand.grid(Ship = unique(fin_ddf$data$Ship),
                       Vis  = quantile(fin_ddf$data$Vis, 0.5),
                       LnTotSS  = quantile(fin_ddf$data$LnTotSS,  0.5),
                       Beauf  = quantile(fin_ddf$data$Beauf,  0.5))
plot_df$SppMax <- "074"
pal <- brewer.pal(6, "Set1")
add_df_covar_line(fin_ddf, plot_df, col=pal, lty=1)
legend(x="topright", legend=plot_df$Ship, col=pal, lty=1, title="Vessel", inset=c(0.05, 0.05))


# by visibility
plot(fin_ddf, showpoints=FALSE)
plot_df <- expand.grid(Ship = "DSJ",
                       Vis  = 1:6,
                       LnTotSS  = quantile(fin_ddf$data$LnTotSS,  0.5),
                       Beauf  = quantile(fin_ddf$data$Beauf,  0.5))
plot_df$SppMax <- "074"
pal <- brewer.pal(6, "YlGnBu")
add_df_covar_line(fin_ddf, plot_df, col=pal, lty=1)
legend(x="topright", legend=plot_df$Vis, col=pal, lty=1, title="Visibility", inset=c(0.05, 0.05))


# school size
plot(fin_ddf, showpoints=FALSE)
plot_df <- expand.grid(Ship = "DSJ",
                       Vis  = quantile(fin_ddf$data$Vis, 0.5),
                       LnTotSS  = log(c(1, 2, 3)),
                       Beauf  = quantile(fin_ddf$data$Beauf,  0.5))
plot_df$SppMax <- "074"
pal <- brewer.pal(3, "YlGnBu")
add_df_covar_line(fin_ddf, plot_df, col=pal, lty=1)
legend(x="topright", legend=exp(plot_df$LnTotSS), col=pal, lty=1, title="School size", inset=c(0.05, 0.05))


# Beaufort
plot(fin_ddf, showpoints=FALSE)
plot_df <- expand.grid(Ship = "DSJ",
                       Vis  = quantile(fin_ddf$data$Vis, 0.5),
                       LnTotSS  = quantile(fin_ddf$data$LnTotSS,  0.5),
                       Beauf  = 0:6)
plot_df$SppMax <- "074"
pal <- brewer.pal(7, "YlGnBu")
add_df_covar_line(fin_ddf, plot_df, col=pal, lty=1)
legend(x="topright", legend=plot_df$Beauf, col=pal, lty=1, title="Beaufort", inset=c(0.05, 0.05))


dev.off()
