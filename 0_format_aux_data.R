# auxiliary data formatting

# get the CCE polygon
cce_poly <- read.csv("data/CCE_StudyArea_noIslands_noBays.csv")
# longitude on the same scale as the data
cce_poly$x <- cce_poly$x-360

# load the prediction grid areas
pred_areas <- read.csv("data/Grid_Lat30to48_Lon226to244.5_Step0.1withArea.csv")

# save data
save(cce_poly, pred_areas, file="RData/0_format_aux_data.RData")
