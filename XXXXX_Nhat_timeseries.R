
load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")
load("RData/4_process_uncertainty.RData")

library(ggplot2)
library(mapdata)

# Nhat time series
# need to fiddle with dates to get the data nicely plotable
Nhat <- read.csv("out/Nhat_ests.csv")
Nhat$X <- NULL
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

# Yearlies for comparison with Nadeem
#nadeem_barlow <- read.csv("data/nadeem2016_barlow.csv")
nadeem_direct <- read.csv("data/nadeem2016_direct.csv")
moore <- read.csv("data/moore2011.csv")


# Make a big old data.frame
library(tidyr)
library(dplyr)

Nhat$date <- dates
Nhat_long<- gather(Nhat, "simulation", "Abundance", -date)
Nhat_long$Year <- year(Nhat_long$date)


yearlies <- Nhat_long %>%
  group_by(Year) %>%
  summarize(perc20  = quantile(Abundance, 0.2),
            Mean    = mean(Abundance),
            Median  = median(Abundance),
            lower95 = quantile(Abundance, 0.025),
            upper95 = quantile(Abundance, 0.975))

yearlies$model <- "Miller"
#nadeem_barlow$model <- "Nadeem (Barlow g(0))"
#nadeem_barlow$CV <- NULL
#nadeem_barlow$Mode <- NULL
nadeem_direct$model <- "Nadeem (direct g(0))"
nadeem_direct$CV <- NULL
nadeem_direct$Mode <- NULL
moore$model <- "Moore"
moore$CV <- NULL
moore$Mode <- NULL
moore$perc20 <- NA


#yearlies <- rbind(nadeem_direct, nadeem_barlow, moore, as.data.frame(yearlies))
yearlies <- rbind(nadeem_direct, moore, as.data.frame(yearlies))

nadeem_comp <- ggplot(yearlies, aes(x=Year)) +
  geom_point(aes(y=Mean, colour=model),position = position_dodge(width = 0.9)) +
  geom_point(aes(y=perc20, colour=model), pch=4, size=3,position = position_dodge(width = 0.9)) +
  geom_linerange(aes(ymin=lower95, ymax=upper95, colour=model),position = position_dodge(width = 0.9)) +
  labs(x="Year", y="Abundance") +
  scale_x_continuous(breaks=unique(yearlies$Year)) +
  theme_minimal()

print(nadeem_comp)

ggsave(nadeem_comp, file="figures/nadeem_comp.pdf", width=8, height=6)




