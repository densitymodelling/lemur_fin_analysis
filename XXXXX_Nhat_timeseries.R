
load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")
load("RData/4_process_uncertainty.RData")

library(ggplot2)
library(mapdata)

# Nhat time series
# need to fiddle with dates to get the data nicely plotable
Nhat <- read.csv("out/Nhat_ests.csv")
#Nhat_fix <- boxplot.stats(as.matrix(Nhat))
Nhat2 <- Nhat
#Nhat2[Nhat2>Nhat_fix$stats[5]] <- NA
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
  geom_ribbon(aes(ymax=upper, ymin=lower), fill="lightgrey") +
  geom_line(aes(y=means)) +
  facet_wrap(~year, scales = "free_x") +
  labs(x="Date", y="Abundance") +
  theme_minimal()
p_Nhat

#ggsave(p_Nhat, file="figures/Nhat.pdf", width=8, height=7)

