library(caret)
library(randomForest)
library(e1071) 
library(raster)
library(maptools)
library(RStoolbox)
library(rgeos)
library(snow)

mnftextuer <- brick(file.choose())

reflectance <- brick(file.choose())

clayRatio <- reflectance$SWIR.1..1.609000.Micrometers. / reflectance$SWIR.2..2.201000.Micrometers.
FerrousOxideRatio <- reflectance$SWIR.1..1.609000.Micrometers. / reflectance$Near.Infrared..NIR...0.864600.Micrometers.
IronOxideRatio <- reflectance$Red..0.654600.Micrometers./reflectance$Blue..0.482600.Micrometers.

toClass <- brick(list(mnftextuer, clayRatio, FerrousOxideRatio, IronOxideRatio))

xx <- readShapePoly( file.choose())
projection(xx) <- projection(image)

no_cores <- detectCores() - 1
beginCluster(no_cores)
values <- clusterR(toClass, extract, args=list(xx, df = TRUE))
endCluster()

values <- extract(toClass, xx, df = TRUE)
classes <- data.frame(ID = 1:length(xx@data$CLASS_NAME), xx@data)
classarr <-

values <- data.frame(Class = classes$CLASS_NAME[values$ID], values)
valuesBackup <- values
values <- values[,-1]

#values <- data.frame(values, Class = xx@data$Class[values$ID])
table(values$Class)

for (i in 1:dim(values)[1]){
  for (j in 1:dim(values)[2]){
    if (is.infinite(values[i,j])){values[i,j] <- NA}
  }
}
Nafill <- preProcess(values, method = "bagImpute")
valuesNAfilled <- predict(Nafill, values)
sum(is.na(valuesNAfilled))

kclus <- kmeans(valuesNAfilled[, -c(1,dim(valuesNAfilled)[2])], 
                centers = 6)
table(valuesNAfilled$Class, kclus$cluster)
table(valuesNAfilled$Class)


set.seed(1235)
intrain <- createDataPartition(values$Class, p=.7, list=F)
training <- valuesNAfilled[intrain, ]
names(training)

testing <- valuesNAfilled[-intrain,]

#data$Class <- as.factor(data$Class)

#training$Class <- as.factor(training$Class)
#testing$Class <- as.factor(testing$Class)

modelRF <- train(Class ~., data=training, method="rf")
pred <- predict(modelRF, testing)
predraster <- predict(toClass, modelRF,
                      filename = file.choose(), na.rm=F,inf.rm = TRUE)
confusionMatrix(pred, testing$Class)

sapply(11:30,function(i) sum(is.na(getValues(subset(toClass,i)))))

texture <-subset(toClass, 4:27)
texturepca <- rasterPCA(texture, )