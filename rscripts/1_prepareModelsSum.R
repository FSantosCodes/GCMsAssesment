library(terra)
library(sf)
library(parallel)
library(doParallel)
library(foreach)
library(tictoc)

#study areas shapefile
studyAreas_shp <- "F:/DATA_AUX1/sensitivityModels/1_data/shp/biogeographic_sectors_GWS.shp"
#input raw future climate folder
#input_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp126"
#input_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp245"
#input_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp370"
input_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp585"
#input raw historical climate folder
input_climHistorical <-  "F:/DATA_AUX1/sensitivityModels/1_data/raw/historical"
#process historical climate?
processHistorical <- F
#number of cores
ncores <- 6
#output study area folders for future climate
#output_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/ssp126"
#output_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/ssp245"
#output_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/ssp370"
output_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/ssp585"
#output study area folders for historical climate
output_climHistorical <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/historical"

#### PREPARE DATA ####

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
    prec.sum <- crop(prec.data,ext(areas.shp[i]))
    prec.sum <- mask(prec.sum,areas.shp[i])
    prec.sum <- sum(prec.sum,na.rm=T)
    prec.sum.val <- values(prec.sum)
    prec.sum.val <- prec.sum.val[!is.na(prec.sum.val)]
    #tmax
    tmax.mean <- crop(tmax.data,ext(areas.shp[i]))
    tmax.mean <- mask(tmax.mean,areas.shp[i])
    tmax.mean <- mean(tmax.mean,na.rm=T)
    tmax.mean.val <- values(tmax.mean)
    tmax.mean.val <- tmax.mean.val[!is.nan(tmax.mean.val)]
    #tmin
    tmin.mean <- crop(tmin.data,ext(areas.shp[i]))
    tmin.mean <- mask(tmin.mean,areas.shp[i])
    tmin.mean <- mean(tmin.mean,na.rm=T)
    tmin.mean.val <- values(tmin.mean)
    tmin.mean.val <- tmin.mean.val[!is.na(tmin.mean.val)]
    #save rasters
    out.file <- paste0(hist.folders[i],"/hist_param.tif")
    hist.sum <- rast(list(prec.sum,tmax.mean,tmin.mean))
    names(hist.sum) <- c("prec","tmax_mn","tmin_mn")
    writeRaster(hist.sum,out.file,datatype="FLT4S",overwrite=T)
    rm(prec.sum,tmax.mean,tmin.mean,hist.sum)
    #data table
    code.i <- as.data.frame(areas.shp[i])$CODE
    hist.df <- data.frame(code=code.i,
                          prec=round(prec.sum.val,1),
                          tmax_mn=round(tmax.mean.val,1),
                          tmin_mn=round(tmin.mean.val,1))
    #save tables
    hist.df$model <- "historical"
    out.file <- paste0(hist.folders[i],"/hist_tbl.csv")
    write.csv(hist.df,out.file,row.names=F)
  }
  rm(hist.df,prec.data,tmax.data,tmin.data,prec.sum.val,tmax.mean.val,tmin.mean.val)
}
#list models & split by models
fut.files <- list.files(input_climFuture,full.names=T,pattern=".tif$")
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
  x.ope <- rast(x)
  tryCatch({
    #operate study areas
    lapply(1:nrow(areas.shp),function(y){
      #operate mean raster
      y.ope <- crop(x.ope,ext(areas.shp[y]))
      y.ope <- mask(y.ope,areas.shp[y])
      if(param.id=="prec"){
        y.ope <- sum(y.ope,na.rm=T)
      }else{
        #for other params such as temperature
        y.ope <- mean(y.ope,na.rm=T)
      }
      names(y.ope) <- paste0(param.id,"_mn")
      #operate difference
      y.diff <- hist.data[[y]][param.id]
      y.diff <- y.ope - y.diff
      names(y.diff) <- "diff"
      #save raster
      y.rast <- rast(list(y.ope,y.diff))
      out.file <- gsub("wc2.1_2.5m_","",basename(x))
      out.file <- paste0(fut.folders[y],"/",out.file)
      writeRaster(y.rast,out.file,datatype="FLT4S",overwrite=T)
      #extract values
      y.ope.val <- values(y.ope)
      y.ope.val <- y.ope.val[!is.na(y.ope.val)]
      y.ope.val <- y.ope.val[!is.nan(y.ope.val)] 
      y.ope.val <- round(y.ope.val,1)
      y.diff.val <- values(y.diff)
      y.diff.val <- y.diff.val[!is.na(y.diff.val)]
      y.diff.val <- y.diff.val[!is.nan(y.diff.val)]
      y.diff.val <- round(y.diff.val,1)
      code.y <- as.data.frame(areas.shp[y])$CODE
      y.df <- data.frame(code=code.y,
                         param=param.id,
                         model=model.id,
                         ssp=ssp.id,
                         step=step.id,
                         value=y.ope.val,
                         diff=y.diff.val)
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
process.out <- do.call("rbind.data.frame",process.out)
out.file <- paste0(output_climFuture,"/processingReport.csv")
write.csv(process.out,out.file,row.names = F)
