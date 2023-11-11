library(terra)
library(xlsx)

#input historical folder
historical_folder <- "F:/DATA_AUX1/sensitivityModels/1_data/raw/historical"
#input average climate models folders
climate_folders <- c("F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp126",
                     "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp245",
                     "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp370",
                     "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp585")
#models ranking
model_rank <- "F:/DATA_AUX1/sensitivityModels/3_plots/annex2_ranking.xlsx"
#study area 
studyArea <- "F:/DATA_AUX1/sensitivityModels/1_data/shp/regionalArea.shp"
#output folder
output_folder <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/extremes2"

#### PREPARE DATA ####

#read ranks and get model eeeeextremes!
model.rank <- read.xlsx(model_rank,1)
model.rank <- model.rank[order(model.rank$global),]
model.rank <- model.rank[model.rank$model!="SPRACC",]

#version based in <0
model.extreme <- model.rank
model.extreme$type <- NA
model.extreme$type[model.extreme$global<0] <- "dry"
model.extreme$type[model.extreme$global>0] <- "wet"
model.extreme <- split(model.extreme,model.extreme$type)

#version based in 5 extremes
# model.extreme <- model.rank[c(1:5,22:26),1:2]
# model.extreme$type <- NA
# model.extreme$type[1:5] <- "dry"
# model.extreme$type[6:10] <- "wet"
# model.extreme <- split(model.extreme,model.extreme$type)

#read study area
area.shp <- vect(studyArea)

#### PROCESS HISTORICAL ####

#get files
hist.files <- list.files(historical_folder,full.names=T,pattern=".tif$")
hist.files <- grep("prec_",hist.files,value=T)
#read and crop
hist.rast <- rast(hist.files)
hist.rast <- crop(hist.rast,ext(area.shp))
hist.rast <- mask(hist.rast,area.shp)
hist.rast <- sum(hist.rast,na.rm=T)
#save
# out.file <- paste0(output_folder,"/historical_prec.tif")
# writeRaster(hist.rast,out.file,datatype="FLT4S",overwrite=T)

#### PROCESS FUTURE DRY ####

#process files
lapply(climate_folders,function(x){
  #get
  x.files <- list.files(x,full.names=T,pattern=".tif$")
  x.files <- grep("prec_",x.files,value=T)
  target.models <- model.extreme[[1]][,"model"]
  x.models <- sapply(strsplit(basename(x.files),"_"),"[[",4)
  x.models <- x.models %in% target.models
  x.files <- x.files[x.models]
  #read and sum
  x.rast <- lapply(x.files,function(y){
    y.rast <- rast(y)
    y.rast <- crop(y.rast,ext(area.shp))
    y.rast <- mask(y.rast,area.shp)
    y.rast <- sum(y.rast,na.rm=T)
    return(y.rast)
  })
  #average
  x.ave <- rast(x.rast)
  x.ave <- mean(x.ave)
  #difference
  x.diff <- x.ave - hist.rast
  #save
  out.file <- unique(model.extreme[[1]]$type)
  out.file <- paste0(out.file,"_",basename(x),".tif")
  names(x.diff) <- gsub(".tif","",out.file)
  out.file <- paste0(output_folder,"/",out.file)
  writeRaster(x.diff,out.file,datatype="FLT4S",overwrite=T)
  return(out.file)
})

#### PROCESS FUTURE WET ####

#process files
lapply(climate_folders,function(x){
  #get
  x.files <- list.files(x,full.names=T,pattern=".tif$")
  x.files <- grep("prec_",x.files,value=T)
  target.models <- model.extreme[[2]][,"model"]
  x.models <- sapply(strsplit(basename(x.files),"_"),"[[",4)
  x.models <- x.models %in% target.models
  x.files <- x.files[x.models]
  #read and sum
  x.rast <- lapply(x.files,function(y){
    y.rast <- rast(y)
    y.rast <- crop(y.rast,ext(area.shp))
    y.rast <- mask(y.rast,area.shp)
    y.rast <- sum(y.rast,na.rm=T)
    return(y.rast)
  })
  #average
  x.ave <- rast(x.rast)
  x.ave <- mean(x.ave)
  #difference
  x.diff <- x.ave - hist.rast
  #save
  out.file <- unique(model.extreme[[2]]$type)
  out.file <- paste0(out.file,"_",basename(x),".tif")
  names(x.diff) <- gsub(".tif","",out.file)
  out.file <- paste0(output_folder,"/",out.file)
  writeRaster(x.diff,out.file,datatype="FLT4S",overwrite=T)
  return(out.file)
})

