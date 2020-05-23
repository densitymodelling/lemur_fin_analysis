# make time series plots
#  1. monthly estimates per year
#  2. compare yearly abundances with those in Moore and Barlow (2011) and Nadeem et al (2016)


library(ggplot2)
library(mapdata)
library(tidyr)
library(dplyr)
library(lubridate)

load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")
load("RData/4_process_uncertainty.RData")

# load the set of dates, which we'll use for both plots
out_pred_files <- dir("out", pattern="_pred.csv", full.names = TRUE)
dates <- sub("out/CCE_0.1deg_", "", out_pred_files)
dates <- sub("_pred.csv","",dates)
dates <- ymd(dates)

# Nhat time series

Nhat <- read.csv("out/Nhat_ests.csv")
Nhat$X <- NULL
#Nhat_fix <- boxplot.stats(as.matrix(Nhat))
Nhat2 <- Nhat
#Nhat2[Nhat2>Nhat_fix$stats[5]] <- NA


Nhat$date <- dates
Nhat <- Nhat %>%
  # make a long rather than wide data.frame
  pivot_longer(-date, names_to="simulation") %>%
  # don't care about the simulation column
  select(date, value) 

# tidy the data
plot_Nhat <- Nhat %>%
  # summarize by year-month
  group_by(year = year(date), month=month(date)) %>%
  # make the summaries
  summarize(Abundance = mean(value, na.rm=TRUE),
            upper     = quantile(value, probs=0.975, na.rm=TRUE),
            lower     = quantile(value, probs=0.025, na.rm=TRUE))


p_Nhat <- ggplot(plot_Nhat, aes(x=month))+
#  geom_ribbon(aes(ymax=upper, ymin=lower), fill="lightgrey") +
#  geom_line(aes(y=Abundance)) +
  geom_linerange(aes(ymax=upper, ymin=lower)) +
  geom_point(aes(y=Abundance)) +
  facet_wrap(~year, nrow=1, scales = "free_x") +
  scale_x_continuous(breaks=6:12, labels=month.abb[6:12]) +
  labs(x="Date", y="Abundance") +
  theme_minimal()
#p_Nhat

ggsave(p_Nhat, file="figures/Nhat.pdf", width=8, height=7)


p_violin <- ggplot(Nhat, aes(x=month(date))) +
  geom_violin(aes(y=value, group=month(date))) +
  facet_wrap(~year(date), scales = "free_x") +
  scale_x_continuous(breaks=6:12, labels=month.abb[6:12]) +
  labs(x="Date", y="Abundance") +
  theme_minimal()
#p_violin

ggsave(p_violin, file="figures/Nhat_violin.pdf", width=8, height=7)

# Yearlies for comparison with Nadeem
#nadeem_barlow <- read.csv("data/nadeem2016_barlow.csv")
nadeem_direct <- read.csv("data/nadeem2016_direct.csv")
moore <- read.csv("data/moore2011.csv")


# Make a big old data.frame
library(tidyr)
library(dplyr)

Nhat_long<- gather(Nhat, "simulation", "Abundance", -date)
Nhat_long$Year <- year(Nhat_long$date)


yearlies <- Nhat_long %>%
  group_by(Year) %>%
  summarize(perc20  = quantile(Abundance, 0.2),
            Mean    = mean(Abundance),
            Median  = median(Abundance),
            lower95 = quantile(Abundance, 0.025),
            upper95 = quantile(Abundance, 0.975))

yearlies$model <- "This article"
#nadeem_barlow$model <- "Nadeem (Barlow g(0))"
#nadeem_barlow$CV <- NULL
#nadeem_barlow$Mode <- NULL
#nadeem_direct$model <- "Nadeem (direct g(0))"
nadeem_direct$model <- "Nadeem et al. (2016)"
nadeem_direct$CV <- NULL
nadeem_direct$Mode <- NULL
moore$model <- "Moore and Barlow (2011)"
moore$CV <- NULL
moore$Mode <- NULL
moore$perc20 <- NA


#yearlies <- rbind(nadeem_direct, nadeem_barlow, moore, as.data.frame(yearlies))
yearlies <- rbind(nadeem_direct, moore, as.data.frame(yearlies))

nadeem_comp <- ggplot(yearlies, aes(x=Year)) +
  geom_point(aes(y=Mean, colour=model),position = position_dodge(width = 0.9)) +
  geom_point(aes(y=perc20, colour=model), pch=4, size=3,position = position_dodge(width = 0.9)) +
  geom_linerange(aes(ymin=lower95, ymax=upper95, colour=model),position = position_dodge(width = 0.9)) +
  labs(x="Year", y="Abundance", fill="Estimate") +
  scale_x_continuous(breaks=unique(yearlies$Year)) +
  theme_minimal()

#print(nadeem_comp)

ggsave(nadeem_comp, file="figures/nadeem_comp.pdf", width=8, height=6)




