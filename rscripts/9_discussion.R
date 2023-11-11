library(xlsx)


modelsFeatures <- "F:/DATA_AUX1/sensitivityModels/1_data/modelsFeatures/modelsFeatures.csv"
modelsRanking <- "F:/DATA_AUX1/sensitivityModels/3_plots/annex2_ranking.xlsx"

#read
models.features <- read.csv(modelsFeatures)
models.features[models.features == "-"] <- NA
models.features$Model <- gsub(" ","",models.features$Model)
models.ranking <- read.xlsx(modelsRanking,1)
#average resolution
models.features$AVG <- apply(models.features,1,function(x){
  x.data <- na.omit(as.numeric(x[4:12]))
  x <- sum(x.data) / length(x.data)
  return(floor(x))
})


#### MODELS COMPONENTS ####

#select increase
models.select <- models.ranking[models.ranking$global>0,]
models.increase <- models.features[models.features$Model %in% models.select$model,]
models.increase <- models.increase[order(match(models.increase$Model,models.select$model)),]
models.increase <- cbind(models.increase,models.select[,c(1,3)])
models.increase <- lapply(models.increase[,4:11],function(x){
  x <- as.data.frame(table(x,useNA="always"))
  x$per <- round((x$Freq*100)/sum(x$Freq),1)
  x$x <- as.character(x$x)
  x[nrow(x),1] <- "no"
  x.data <- data.frame(x="yes",Freq=sum(x[1:(nrow(x)-1),2]),per=sum(x[1:(nrow(x)-1),3]))
  return(x.data)
})
models.increase <- do.call("rbind.data.frame",models.increase)
models.increase$Var1 <- rownames(models.increase)
rownames(models.increase) <- NULL
#select decrease
models.select <- models.ranking[models.ranking$global<0,]
models.decrease <- models.features[models.features$Model %in% models.select$model,]
models.decrease <- models.decrease[order(match(models.decrease$Model,models.select$model)),]
models.decrease <- cbind(models.decrease,models.select[,c(1,3)])
models.decrease <- lapply(models.decrease[,4:11],function(x){
  x <- as.data.frame(table(x,useNA="always"))
  x$per <- round((x$Freq*100)/sum(x$Freq),1)
  x$x <- as.character(x$x)
  x[nrow(x),1] <- "noData"
  x.data <- data.frame(x="yes",Freq=sum(x[1:(nrow(x)-1),2]),per=sum(x[1:(nrow(x)-1),3]))
  return(x.data)
})
models.decrease <- do.call("rbind.data.frame",models.decrease)
models.decrease$Var1 <- rownames(models.decrease)
rownames(models.decrease) <- NULL
models.decrease[order(models.decrease$per),]
models.increase[order(models.increase$per),]

#### STUDY AREA LARGEST VALUES POSITIVE ####

#global 
models.ranking[order(models.ranking$global,decreasing=T)[1:2],]
#amz
models.ranking[order(models.ranking$AMZ,decreasing=T)[1:2],c("model","AMZ")]
#and
models.ranking[order(models.ranking$AND,decreasing=T)[1:2],c("model","AND")]
#cho
models.ranking[order(models.ranking$CHO,decreasing=T)[1:2],c("model","CHO")]
#EQP
models.ranking[order(models.ranking$EQP,decreasing=T)[1:2],c("model","EQP")]
#gal
models.ranking[order(models.ranking$GAL,decreasing=T)[1:2],c("model","GAL")]

#### STUDY AREA LARGEST VALUES NEGATIVE ####

#global 
models.ranking[order(models.ranking$global,decreasing=F)[1:2],]
#amz
models.ranking[order(models.ranking$AMZ,decreasing=F)[1:2],c("model","AMZ")]
#and
models.ranking[order(models.ranking$AND,decreasing=F)[1:2],c("model","AND")]
#cho
models.ranking[order(models.ranking$CHO,decreasing=F)[1:2],c("model","CHO")]
#EQP
models.ranking[order(models.ranking$EQP,decreasing=F)[1:2],c("model","EQP")]
#gal
models.ranking[order(models.ranking$GAL,decreasing=F)[1:2],c("model","GAL")]

#### MODEL FEATURES SPATIAL RESOLUTION ####

models.select <- models.ranking[models.ranking$global>0,]
models.increase <- models.features[models.features$Model %in% models.select$model,]
models.increase <- models.increase[order(match(models.increase$Model,models.select$model)),]
models.increase <- models.increase[1:5,]
lapply(models.increase[,4:ncol(models.increase)],table)


models.select <- models.ranking[models.ranking$global<0,]
models.decrease <- models.features[models.features$Model %in% models.select$model,]
models.decrease <- models.decrease[order(match(models.decrease$Model,models.select$model),decreasing = T),]
models.decrease <- models.decrease[1:5,]
lapply(models.decrease[,4:ncol(models.decrease)],table)

######## WET ###########

5 #ATM refers to atmosphere physical parameters, 
5 #LND refers to the non-ocean parts and includes factors such as topography, vegetation cover, and soil properties.
5 #OCN refers to the behavior of the ocean currents, temperatures, and properties such as salinity and dissolved gases. 
5 #OSW refers to frozen seawater that floats on the surface of the ocean. 

3 #AERs refers to aerosols or small particles suspended in the atmosphere. 
2 #ATC refers to atmospheric chemical composition and air quality. 
1 #OCB refers to ocean biogeochemistry. 
0 #LNI refers to the dynamics of ice sheets, glaciers, and ice caps on land. 

#SPA refers to the nominal spatial resolution of the model.

######## DRY ###########

5 #ATM refers to atmosphere physical parameters, 
5 #LND refers to the non-ocean parts and includes factors such as topography, vegetation cover, and soil properties.
5 #OCN refers to the behavior of the ocean currents, temperatures, and properties such as salinity and dissolved gases. 
5 #OSW refers to frozen seawater that floats on the surface of the ocean. 

5 #AERs refers to aerosols or small particles suspended in the atmosphere. 
5 #OCB refers to ocean biogeochemistry. 

3 #ATC refers to atmospheric chemical composition and air quality. 
3 #LNI refers to the dynamics of ice sheets, glaciers, and ice caps on land. 

#SPA refers to the nominal spatial resolution of the model.





#AERs refers to aerosols or small particles suspended in the atmosphere. 
#ATM refers to atmosphere physical parameters, 
#ATC refers to atmospheric chemical composition and air quality. 
#LND refers to the non-ocean parts and includes factors such as topography, vegetation cover, and soil properties.
#LNI refers to the dynamics of ice sheets, glaciers, and ice caps on land. 
#OCN refers to the behavior of the ocean currents, temperatures, and properties such as salinity and dissolved gases. 
#OCB refers to ocean biogeochemistry. 9OSW refers to frozen seawater that floats on the surface of the ocean. 
#SPA refers to the nominal spatial resolution of the model.

