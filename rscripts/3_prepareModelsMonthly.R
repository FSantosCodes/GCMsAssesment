library(terra)
library(sf)
library(parallel)
library(doParallel)
library(foreach)
library(tictoc)

#study areas shapefile
studyAreas_shp <- "F:/DATA_AUX1/sensitivityModels/1_data/shp/biogeographic_sectors_GWS.shp"
#input raw data future climate folder
input_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp585"
#input raw data historical climate folder
input_climHistorical <-  "F:/DATA_AUX1/sensitivityModels/1_data/raw/historical"
#process historical climate?
processHistorical <- F
#number of cores
ncores <- 6
#output study area folders for future climate
output_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/monthly/ssp585"
#output study area folders for historical climate
output_climHistorical <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/monthly/historical"

#### PREPARE DATA ####

#list models & split by models
fut.files <- list.files(input_climFuture,full.names=T,pattern=".tif$")
#check if it was already processed
if(file.exists(paste0(output_climFuture,"/processingReport.csv"))){
  processed.fail <- read.csv(paste0(output_climFuture,"/processingReport.csv"))
  processed.fail <- processed.fail[!processed.fail$success,"file"]
  fut.files <- processed.fail
  print("processing only failure files...")
}
#get historical
if(processHistorical){
  areas.shp <- vect(studyAreas_shp)
  areas.shp <- areas.shp[order(areas.shp$GRIDCODE),]
  hist.folders <- list.files(output_climHistorical,full.names=T)
  hist.files <- list.files(input_climHistorical,full.names=T,pattern=".tif$")
  prec.data <- rast(grep("prec_",hist.files,value=T))
  tmax.data <- rast(grep("tmax_",hist.files,value=T))
  tmin.data <- rast(grep("tmin_",hist.files,value=T))
  for(i in 1:nrow(areas.shp)){
    #prec
    prec.months <- crop(prec.data,ext(areas.shp[i]))
    prec.months <- mask(prec.months,areas.shp[i])
    names(prec.months) <- gsub("wc2.1_2.5m_","",names(prec.months))
    prec.months.val <- as.data.frame(values(prec.months))
    prec.months.val <- prec.months.val[complete.cases(prec.months.val),]
    #tmax
    tmax.months <- crop(tmax.data,ext(areas.shp[i]))
    tmax.months <- mask(tmax.months,areas.shp[i])
    names(tmax.months) <- gsub("wc2.1_2.5m_","",names(tmax.months))
    tmax.months.val <- as.data.frame(values(tmax.months))
    tmax.months.val <- tmax.months.val[complete.cases(tmax.months.val),]
    #tmin
    tmin.months <- crop(tmin.data,ext(areas.shp[i]))
    tmin.months <- mask(tmin.months,areas.shp[i])
    names(tmin.months) <- gsub("wc2.1_2.5m_","",names(tmin.months))
    tmin.months.val <- as.data.frame(values(tmin.months))
    tmin.months.val <- tmin.months.val[complete.cases(tmin.months.val),]
    #save rasters
    out.file <- paste0(hist.folders[i],"/hist_param.tif")
    hist.months <- rast(list(prec.months,tmax.months,tmin.months))
    writeRaster(hist.months,out.file,datatype="FLT4S",overwrite=T)
    rm(prec.months,tmax.months,tmin.months,hist.months)
    #data table
    hist.df <- cbind(prec.months.val,
                     round(tmax.months.val,1),
                     round(tmin.months.val,1)
                     )
    hist.df$code <- as.data.frame(areas.shp[i])$CODE
    hist.df$model <- "historical"
    hist.df <- hist.df[,c(37,38,1:36)]
    #save table
    out.file <- paste0(hist.folders[i],"/hist_tbl.csv")
    write.csv(hist.df,out.file,row.names=F)
  }
  rm(hist.df,prec.data,tmax.data,tmin.data,prec.months.val,tmax.months.val,tmin.months.val)
}
#output study area folders
fut.folders <- list.files(output_climFuture,full.names=T)
#future climate function
futClim <- function(x){
  #get model info
  param.id <- gsub("wc2.1_2.5m_|.tif","",basename(x))
  model.id <- unlist(strsplit(param.id,"_"))[2]
  ssp.id <- unlist(strsplit(param.id,"_"))[3]
  step.id <- unlist(strsplit(param.id,"_"))[4]
  param.id <- unlist(strsplit(param.id,"_"))[1]
  #operate
  x.months <- rast(x)
  tryCatch({
    #operate study areas
    lapply(1:nrow(areas.shp),function(y){
      #operate mean raster
      y.months <- crop(x.months,ext(areas.shp[y]))
      y.months <- mask(y.months,areas.shp[y])
      names(y.months) <- gsub("wc2.1_2.5m_","",names(y.months))
      #operate difference
      y.diff <- hist.data[[y]]
      y.diff <- y.diff[[grep(param.id,names(y.diff))]]
      y.diff <- y.months - y.diff
      names(y.diff) <- gsub(param.id,"diff",names(y.diff))
      #save raster
      y.rast <- rast(list(y.months,y.diff))
      out.file <- gsub("wc2.1_2.5m_","",basename(x))
      out.file <- paste0(fut.folders[y],"/",out.file)
      writeRaster(y.rast,out.file,datatype="FLT4S",overwrite=T)
      #extract values
      y.months.val <- as.data.frame(values(y.months))
      y.months.val <- y.months.val[complete.cases(y.months.val),]
      y.diff.val <- as.data.frame(values(y.diff))
      y.diff.val <- y.diff.val[complete.cases(y.diff.val),]
      if(param.id!="prec"){
        y.months.val <- round(y.months.val,1)
        y.diff.val <- round(y.diff.val,1)
      }
      #data table
      y.df <- cbind(y.months.val,y.diff.val)
      y.df$code <- as.data.frame(areas.shp[y])$CODE
      y.df$param <- param.id
      y.df$model <- model.id
      y.df$ssp <- ssp.id
      y.df$step <- step.id
      y.df <- y.df[,c(25:29,1:24)]
      #save table
      out.file <- paste0(gsub("wc2.1_2.5m_|.tif","",basename(x)),".csv")
      out.file <- paste0(fut.folders[y],"/",out.file)
      write.csv(y.df,out.file,row.names=F)
    })
    success <- data.frame(file=x,success=T)
    return(success)
  },error = function(e){
    success <- data.frame(file=x,success=F)
    return(success)
  })
}

#### START PROCESSING ####

#cluster
cl <- makeCluster(ncores)
registerDoParallel(cl)
par.libraries <- c("sf","terra","tictoc")
#go processing #   
process.out <- foreach(i=1:length(fut.files),.packages=par.libraries,.errorhandling="stop") %dopar% {
  #start time
  tic()
  #read study area
  areas.shp <- vect(studyAreas_shp)
  areas.shp <- areas.shp[order(areas.shp$GRIDCODE),]
  #read historical data
  hist.data <- list.files(output_climHistorical,full.names=T)
  hist.data <- lapply(hist.data,function(x){
    x.ras <- rast(list.files(x,full.names=T,pattern=".tif$"))
    return(x.ras)
  })
  #apply function
  success <- futClim(fut.files[i])
  #return time
  time.fun <- toc(quiet=T)$callback_msg
  time.fun <- as.numeric(gsub(" sec elapsed","",time.fun))
  success$time <- time.fun
  return(success)
}
stopCluster(cl)
#derive process report
if(!file.exists(paste0(output_climFuture,"/processingReport.csv"))){
  process.out <- do.call("rbind.data.frame",process.out)
  out.file <- paste0(output_climFuture,"/processingReport.csv")
  write.csv(process.out,out.file,row.names = F)
}else{
  print(process.out)
}
