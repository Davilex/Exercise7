Create a model object using lm() using Landsat band reflectance values as predictors for tree cover (VCF). Using the resulting model object, predict VCF values for the Gewata area.



produce one or more plots that demonstrate the relationship between the Landsat bands and the VCF tree cover. What can you conclude from this/these plot(s)?
create an lm() model and show a summary (e.g. using summary()) of the model object you created. Which predictors (bands) are probably most important in predicting tree cover?
plot the predicted tree cover raster and compare with the original VCF raster.
compute the RMSE between your predicted and the actual tree cover values (hint)
are the differences between the predicted and actual tree cover the same for all of the 3 classes we used for the random forest classfication? Using the training polygons from the random forest classification, calculate the RMSE separately for each of the classes and compare. Hint - see ?zonal().

library(raster)

# load data
load("data/GewataB1.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")

load("data/vcfGewata.rda")
vcfGewata[vcfGewata > 100] <- NA
plot(vcfGewata)
summary(vcfGewata)
hist(vcfGewata)

# cell statistics (2 ways)
cellStats(GewataB1, stat=mean)
maxValue(GewataB7)

# summary for a quick overview
summary(GewataB1)
hist(GewataB7)
summary(GewataB5)
summary(GewataB7)

# put the 3 bands into a rasterBrick
gewata <- brick(GewataB1, GewataB5, GewataB7)

# explore the brick
hist(gewata)
hist(gewata, xlim = c(0, 16000), ylim = c(0, 750000), breaks = seq(0, 16000, by = 100))
opar <- par(mfrow = c(1, 1)) # reset plotting window
pairs(gewata)

# make new brick with covariates (combine Gewata with VCF)
covs <- addLayer(gewata, vcfGewata)
names(covs) <- c("Band1", "Band5", "Band7", "VCF")
plot(covs)
par(opar)

