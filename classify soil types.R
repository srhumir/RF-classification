r <- matrix(1:7, 10,10)
r <- raster(r)
r
par(mfrow = c(1, 2))
plot(r)
r2 <- r * (r > 5)
r3 <- r %/% 6
#get classifcation rasters of soil vs urban and soil vs roof. 
#gather soils of both
#classify soil types
library(caret)
library(randomForest)
library(e1071) 
library(raster)
library(maptools)
library(RStoolbox)
library(rgeos)
library(parallel)
#input data
soilurban <- raster(file.choose())
soilroof <- raster(file.choose())
#values over 5 are not soil so convert them to 1 and soils to zero
soilurban <- soilurban %/% 5
soilroof <- soilroof %/% 5
#get sure about NA values
soilurban <- calc(soilurban, sign)
soilurban <- calc(soilroof, function(x) x * (x != -1))
#now that in both rasters, soils are 0 and urbans are 1 make a function that inion soil pixels
sumNA <- function(a){
  #if (sum(is.na(a)) >= 2) return(NA)
  if (!is.na(sum(a))){if (sum(a) > 1) return(NA) else return(1)}
  if (!is.na(a[1])) {if (a[1] > 0) return(NA) else return(1)}
  if (!is.na(a[2])) {if (a[2] > 0) return(NA) else return(1)}
  else return(NA)
}
#use function above to produce soil raster (i.e 1 at soil pixels and NA elswhere)
soil <- calc(stack(soilurban, soilroof), sumNA)
#convert to ploygon
shape <- rasterToPolygons(soil)
#extract 
values <- extract(MNF, shape, df = TRUE)

unique(getValues(soil))
