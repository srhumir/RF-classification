library(caret)
library(randomForest)
library(e1071) 
library(raster)
library(maptools)
library(RStoolbox)
library(rgeos)
library(snow)
library(glcm)
library(parallel)
#loading MNF Image (brick would load 0 as NA)
MNF <- stack(file.choose())
#MNF <- dropLayer(MNF, 1)
names(MNF) <- sapply(1:dim(MNF)[3], function(i) paste("MNF", i, sep="."))


mask <- raster(file.choose())
MNF <- MNF * (mask-1)
#load trainig shp
xx <- readShapePoly( file.choose())
projection(xx) <- projection(MNF)

#compute texture
## Calculate the number of cores
no_cores <- detectCores() - 1
##Create a list of first four MNF bands
rasterlist <- list(raster(MNF, layer = 1), raster(MNF, layer = 2), 
                   raster(MNF, layer = 3), raster(MNF, layer = 4))
## Initiate cluster
Sys.time()
cl <- makeCluster(no_cores)
##Send variable to cluster
clusterExport(cl,c("MNF", "texture", "rasterlist"))
##send packages to cluster
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(glcm))
Sys.time()
##compute texture
texlist <- parLapply(cl, rasterlist, glcm)
Sys.time()
stopCluster(cl)
#

#stack three MNF bands and their textures
toClass <- stack(c(rasterlist, texlist))
Sys.time()

#mask
toClass <- toClass * (mask-1)

#extract values in trainig shapefile
values <- extract(toClass, xx, df = TRUE)
#take out the attributr table of the trainig and assigne ID to polygons
classes <- data.frame(ID = 1:length(xx@data$CLASS_NAME), xx@data)
#Assign class to extracted values
values <- data.frame(Class = classes$CLASS_NAME[values$ID], values)
##when load from disk
#values <- read.csv(file.choose())
# values <- values[,-1]
#drop rows with all zero MNF
values <- values[values[names(values)[3]] > 0 & !is.na(values[names(values)[3]]),]
##a backup
valuesBackup <- values
##No need for ID
values <- values[,-2]
## keep class seperate for speeding up the training and ...
Class <- values$Class
#check fot too many NA
NAs <- sapply(1:dim(toClass)[3],function(i) sum(is.na(getValues(subset(toClass,i)))))
#there are too many na's in bands 11, 19,27. So I omit them
tooNA <- which(NAs > 19000)
toClass2 <- dropLayer(toClass, tooNA)
values <- values[,-(tooNA+1)]

#convert inf to NA for the model to work
for (i in 1:dim(values)[1]){
        for (j in 1:dim(values)[2]){
                if (is.infinite(values[i,j])){values[i,j] <- NA}
        }
}
#fill NAs
##model
Nafill <- preProcess(values, method = "bagImpute")
##fill
valuesNAfilled <- predict(Nafill, values)
##check
sum(is.na(valuesNAfilled))
#ommiting too correlated data
##compute corrolation matrix
corMatrix = cor(valuesNAfilled[,-1])
##get highly coorolated
highlyCorrelated_ <- apply(corMatrix,2, function(x) which(abs(x) >= .95))
##get values to keep. did not use the caret package 
##since I like to keep first three bands any way
keepIndex <- integer()
keepIndex <- unique(sapply(highlyCorrelated_, function(x) c(keepIndex, x[1])))
##drop highly corrolated values
valuesNAfilled <- valuesNAfilled[, (keepIndex+1)]
##drop highly coorolated bands from raster as well
dropIndex <- c(1:dim(toClass2)[3])[-keepIndex]
toClass2 <- dropLayer(toClass2, dropIndex)

#choose trainig and testing
#set.seed(1235)
##index
#intrain <- createDataPartition(values$Class, p=.8, list=F)
##trainig


#training <- valuesNAfilled[intrain, ]
#ClassTrainig <- Class[intrain]
#names(training)
##testing
#testing <- valuesNAfilled[-intrain,]
#ClassTesting <- Class[-intrain]

#defing train control
# define training control
train_control <- trainControl(method="cv", number=10)

#traing RF model
system.time(
        modelRF <- train(valuesNAfilled,droplevels(Class), trControl=train_control,
                         method="rf")
)
#predict on testing
#pred <- predict(modelRF, testing)
#check the accuracy on testing
#confusionMatrix(pred, ClassTesting)

#predict on raster
system.time(
predraster <- predict(toClass2, modelRF,
                      filename = "C:\\Users\\Haniyeh\\Hoa_Binh\\NDBaI\\classification\\classification-soil-urbanRoof2.tif",
                      na.rm=T,inf.rm = TRUE)
)



