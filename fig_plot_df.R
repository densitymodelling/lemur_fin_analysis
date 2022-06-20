# detection function plots

library(mrds)
library(Distance)
library(RColorBrewer)

#load("RData/2_prop_that_var.RData")
load("RData/1_model_and_data.RData")


fin_ddf <- b$ddf

pdf(file="figures/detection_functions.pdf", width=12, height=10)

par(mfrow=c(2,2))

#> table(fin$Ship)
#
# DSJ  END  LAS  MAC  Mc2  OES 
#4947    0    0 2488 5384    0 
fix_Ship <- "Mc2"
fix_Vis <- quantile(fin$Vis, 0.5)
fix_LnTotSS <- log(1)
fix_Beauf <- quantile(fin$Beauf, 0.5)

# by ship
plot(fin_ddf, showpoints=FALSE, subset=SppMax=="074")
plot_df <- expand.grid(Ship = c("DSJ", "MAC", "Mc2"),
                       Vis  = fix_Vis,
                       LnTotSS  = fix_LnTotSS,
                       Beauf  = fix_Beauf)
plot_df$SppMax <- "074"
pal <- brewer.pal(6, "Set1")
add_df_covar_line(fin_ddf, plot_df, col=pal, lty=1)
legend(x="topright",
       legend=c("David Starr Jordan",
                "McArthur", "McArthur II"),
       col=pal, lty=1, title="Vessel", inset=c(0.05, 0.05))


# by visibility
plot(fin_ddf, showpoints=FALSE, subset=SppMax=="074")
plot_df <- expand.grid(Ship = fix_Ship,
                       Vis  = 1:6,
                       LnTotSS  = fix_LnTotSS,
                       Beauf  = fix_Beauf)
plot_df$SppMax <- "074"
pal <- brewer.pal(6, "YlGnBu")
add_df_covar_line(fin_ddf, plot_df, col=pal, lty=1)
legend(x="topright", legend=plot_df$Vis, col=pal, lty=1, title="Visibility", inset=c(0.05, 0.05))


# school size
plot(fin_ddf, showpoints=FALSE, subset=SppMax=="074")
plot_df <- expand.grid(Ship = fix_Ship,
                       Vis  = fix_Vis,
                       LnTotSS  = log(c(1, 2, 3)),
                       Beauf  = fix_Beauf)
plot_df$SppMax <- "074"
pal <- brewer.pal(3, "YlGnBu")
add_df_covar_line(fin_ddf, plot_df, col=pal, lty=1)
legend(x="topright", legend=exp(plot_df$LnTotSS), col=pal, lty=1, title="School size", inset=c(0.05, 0.05))


# Beaufort
plot(fin_ddf, showpoints=FALSE, subset=SppMax=="074")
plot_df <- expand.grid(Ship = fix_Ship,
                       Vis  = fix_Vis,
                       LnTotSS  = fix_LnTotSS,
                       Beauf  = 0:6)
plot_df$SppMax <- "074"
pal <- brewer.pal(7, "YlGnBu")
add_df_covar_line(fin_ddf, plot_df, col=pal, lty=1)
legend(x="topright", legend=plot_df$Beauf, col=pal, lty=1, title="Beaufort", inset=c(0.05, 0.05))


dev.off()
