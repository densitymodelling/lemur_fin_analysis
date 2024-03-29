# investigate 2008 high uncertainty

library(lubridate)
library(patchwork)
library(dsm)
library(ggplot2)
library(mapdata)

# first load up the time series and find out where the maximum is...
Nhat <- read.csv("out/Nhat_ests.csv")
Nhat$X <- NULL
out_pred_files <- dir("out", pattern="_pred.csv", full.names = TRUE)
dates <- sub("out/CCE_0.1deg_", "", out_pred_files)
dates <- sub("_pred.csv","",dates)
dates <- ymd(dates)
plot_Nhat <- data.frame(means = apply(Nhat,1, median, na.rm=TRUE),
                        upper = apply(Nhat,1, quantile,
                                      probs=0.975, na.rm=TRUE),
                        lower = apply(Nhat,1, quantile,
                                      probs=0.025, na.rm=TRUE),
                        date  = dates)

# where is the max?
up <- which.max(plot_Nhat$upper)
plot_Nhat[up,]
#        means    upper    lower       date
# 165 6773.068 20251.32 2903.808 2008-06-26

# get some summaries for that day
hist(unlist(Nhat[dates==ymd("2008-06-26"),]), breaks=100,
     xlab="Abundance")
mean(unlist(Nhat[dates==ymd("2008-06-26"),]))
median(unlist(Nhat[dates==ymd("2008-06-26"),]))



# now let's plot what's going on there

# do some map setup
# load the data for that day
pred_file <- "data/roms_grids/CCE_0.1deg_2008-06-26.csv"
source("support_scripts/prepare_preds.R")
load("RData/0_format_aux_data.RData")
# get USA map for plots
w <- map_data("worldHires", ylim = range(cce_poly$y), xlim = range(cce_poly$x))
# format the data for that day
summary_predgrid <- prepare_preds(pred_file, pred_areas=pred_areas,
                                  poly=cce_poly)

# read-in and calculate the appropriate summaries
out_pred_file <- "out/CCE_0.1deg_2008-06-26_pred.csv"
this_pred <- data.table::fread(out_pred_file, header=TRUE, sep=",")
this_pred <- as.data.frame(this_pred)
summary_predgrid$avv <- apply(this_pred, 1, mean)

# plot mean predictions for this day
summary_predgrid$avv_d <- cut(summary_predgrid$avv,
                              c(0,0.001,.0023,.0036,.0085,.036,1))
# remove NAs
summary_predgrid <- summary_predgrid[!is.na(summary_predgrid$avv_d), ]

# create a plotting object
p_pred <- ggplot(summary_predgrid, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group),
               fill = "grey80") +
  geom_tile(aes(fill=avv_d)) +
  scale_fill_viridis_d() +
  labs(x="", y="", fill="Density") +
  coord_map(ylim=range(summary_predgrid$mlat),
            xlim=range(summary_predgrid$mlon)) +
  theme_minimal()
plot(p_pred)

# now make a plot of the linear predictor per smooth for that day
load("RData/2_prop_that_var.RData")
summary_predgrid$XX <- matrix(0, nrow=nrow(summary_predgrid), ncol=18)

# adapted from dsm::plot_pred_by_term
plot_predgrid <- summary_predgrid
plot_predgrid$off <- 0
preds <- predict(b_vp, plot_predgrid, type = "terms")
preds <- preds[, -c(1, 6)]
plot_data <- c()
for (col in colnames(preds)) {
  plot_data <- rbind(plot_data,
                     cbind.data.frame(plot_predgrid, 
                                      value = preds[, col], term = col))
}

# construct the plot
predplot <- ggplot(plot_data) +
  geom_tile(aes(x = mlon, y = mlat, fill = value)) +
  facet_wrap(~term) +
  scale_fill_viridis_c() +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  coord_map(ylim=range(summary_predgrid$mlat), xlim=range(summary_predgrid$mlon)) +
  labs(x="", y="", fill="Linear\npredictor\nvalue") +
  facet_wrap(~term, nrow=2) +
  theme_minimal()

# fiddle with the ggplot2 object for plotting
predplot$data$term[predplot$data$term=="s(sst)"] <- "Sea surface temperature"
predplot$data$term[predplot$data$term=="s(sst_sd)"] <- "Standard deviation of SST"
predplot$data$term[predplot$data$term=="s(ssh)"] <- "Sea surface height"
predplot$data$term[predplot$data$term=="s(ild)"] <- "Mixed layer depth"
predplot$data$term[predplot$data$term=="te(mlon,mlat)"] <- "Space"

# save that plot for the paper
ggsave(predplot, file="figures/lpplot-2008-06-26.pdf", width=10, height=10)


# make a plot that's just that day's mixed later depth and the predictions
p_ild <- ggplot(summary_predgrid, aes(y=mlat, x=mlon)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_tile(aes(fill=ild)) +
  scale_fill_viridis_c() +
  labs(x="", y="") +
  coord_map(ylim=range(summary_predgrid$mlat), xlim=range(summary_predgrid$mlon)) +
  theme_minimal()


p_pred + p_ild

