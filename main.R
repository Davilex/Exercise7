#title: "Exercise7: Advanced Raster Analysis for the geoScripting course"
#author: 'AD-Scripting: Astrid Bos & David Scholte-Albers'
#date: "Friday, January 16, 2015"

#This script creates a linear model using the Landsat band reflectance values as predictors for tree cover. 
#The resulting model is compared with the values of the VCF product and its accuracy is analysed using the RMSE of the model.

rm(list=ls())

library(raster)
library(gridExtra)

##Preprocessing of the data

# Load band data and training polygons
load("data/GewataB1.rda") 
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda") 
load("data/GewataB7.rda") 
load("data/vcfGewata.rda") #Vegetation Continuous Field
load("data/trainingPoly.rda")

# Remove outliers (Based on histogram and summary of each band)
GewataB1[GewataB1 > 1000] <- NA
GewataB2[GewataB2 > 1300] <- NA
GewataB3[GewataB3 > 1500] <- NA
GewataB5[GewataB5 > 4000] <- NA
GewataB7[GewataB7 > 3000] <- NA

# Remove water, clouds and shadows from VCF
vcfGewata[vcfGewata > 100] <- NA

# Combine bands in a rasterBrick and convert to original values
gewata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
gewata <- calc(gewata, fun=function(x) x / 10000)

##Analysis of relationship between Landsat bands and VCF

# Relationship between bands and VCF
covs <- addLayer(gewata, vcfGewata)
names(covs) <- c("Band1", "Band2", "Band3", "Band4", "Band5", "Band7", "VCF")
pairs(covs)

# Band 3 and 7 have the highest correlation coefficient with the VCF (both 0.89). 
# Surprisingly band 4 (NIR)has the lowest correlation coefficient with the VCF (0.17).

##Linear model

covsdf <- as.data.frame(covs, na.rm=T)
model <- lm(VCF ~ Band1 + Band2 + Band3 + Band4 + Band5 + Band7, data = covsdf)
summary(model)

# In the Pr(>|t|), which represents the variable p-value, all bands show a very low value, 
# which means that the chance that the band is meaningless for the model is very low. 
# All bands have 3 stars, corresponding with a very good significance. For more 
# information on this subject: http://blog.yhathq.com/posts/r-lm-summary.html

# Prediction of tree cover based on the model
vcf_predict <- predict(covs, model, na.rm=T)
vcf_predict[vcf_predict < 0] <- NA
vcf_dif <- vcf_predict - vcfGewata

# Plots of prediction, actual VCF and the difference between those
grid.arrange(spplot(vcf_predict, zlim=c(0,100) ,col.regions=colorRampPalette(c("white", "darkkhaki", "darkgreen"))(100), main="Predicted tree cover"), 
             spplot(vcfGewata, zlim=c(0,100) ,col.regions=colorRampPalette(c("white", "darkkhaki", "darkgreen"))(100), main="Tree cover VCF"), ncol=2)

spplot(vcf_dif, zlim = c(-60,60), main="Predicted Tree cover versus actual VCF", sub="Green represents predictions with a value that is higher than the actual VCF, \nred corresponds with predicted values lower than the VCF", zcol="layer", col.regions=colorRampPalette(c("red", "yellow", "darkgreen"))(100))


##Accuracy assessment of the model using RMSE

# Root mean squared Error for difference in predicted and actual tree cover
dif_df <- as.data.frame(vcf_predict$Treecover - covs$VCF, na.rm=T)
RMSE <- round(sqrt(mean(dif_df ^ 2)), digits=2)
RMSE

# The RMSE for the difference in predicted and actural tree cover is 8.35

##RMSE for different land use classes using training data

# Set up trainings data
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)

# Assign "code" values to raster cells
land_use_classes <- rasterize(trainingPoly, covs$VCF, field='Code')

# Brick with predicted and actual tree cover
pred_act_brick <- brick(covs$VCF, vcf_predict)

# Calculate the mean for every class by the use of zonal()
mean_cover <- zonal(pred_act_brick, land_use_classes, fun='mean',na.rm=T)
mean_cover_df <- as.data.frame(mean_cover)

# Calculate the Root mean squared Error for all of the 3 classes
rmse_mean_cover <- round(sqrt((mean_cover_df$VCF - mean_cover_df$layer)^2), digits=2)
names(rmse_mean_cover) <- c("Crop", "Forest", "Wetlands")
rmse_mean_cover

# The rmse is different for the different landuse classes. Crop has a rmse of 2.55, 
# forest has a rmse of 2.30 and wetlands has a rmse for the prediction of 6.86