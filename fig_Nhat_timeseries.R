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
out_pred_files <- dir("out", pattern="_pred.rds", full.names = TRUE)
dates <- sub("out/CCE_0.1deg_", "", out_pred_files)
dates <- sub("_pred.rds","",dates)
dates <- ymd(dates)

# Nhat time series
Nhat <- read.csv("out/Nhat_ests.csv")
Nhat$X <- NULL
Nhat2 <- Nhat

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
            se        = sqrt(var(value, na.rm=TRUE))) %>%
  mutate(CV    = sqrt((se/Abundance)^2 + g0_CV^2),
         lower = qlnorm(0.05/2, log(Abundance) - 0.5*log(CV^2+1),
                        sqrt(log(CV^2+1))),
         upper = qlnorm(1-0.05/2, log(Abundance) - 0.5*log(CV^2+1),
                        sqrt(log(CV^2+1))))

p_Nhat <- ggplot(plot_Nhat, aes(x=month))+
  geom_linerange(aes(ymax=upper, ymin=lower)) +
  geom_point(aes(y=Abundance)) +
  facet_wrap(~year, nrow=1, scales = "free_x") +
  scale_x_continuous(breaks=6:12, labels=month.abb[6:12], expand=c(.1, .1)) +
  labs(x="", y="Abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=35, hjust=1),
        panel.border = element_rect(colour="black", size=0.5, fill=NA))
p_Nhat

ggsave(p_Nhat, file="figures/Nhat.pdf", width=10, height=5)


# Yearlies for comparison with Nadeem
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
nadeem_direct$model <- "Nadeem et al. (2016)"
nadeem_direct$CV <- NULL
nadeem_direct$Mode <- NULL
moore$model <- "Moore and Barlow (2011)"
moore$CV <- NULL
moore$Mode <- NULL
moore$perc20 <- NA


yearlies <- rbind(nadeem_direct, moore, as.data.frame(yearlies))

nadeem_comp <- ggplot(yearlies, aes(x=Year)) +
  geom_point(aes(y=Mean, colour=model),position = position_dodge(width = 0.9)) +
  geom_linerange(aes(ymin=lower95, ymax=upper95, colour=model),position = position_dodge(width = 0.9)) +
  labs(x="Year", y="Abundance", fill="Estimate", colour="Model") +
  scale_x_continuous(breaks=unique(yearlies$Year)) +
  theme_minimal()

ggsave(nadeem_comp, file="figures/nadeem_comp.pdf", width=8, height=5)

# save data for later
save(yearlies, plot_Nhat, file="RData/Nhat_plot_data.RData")
