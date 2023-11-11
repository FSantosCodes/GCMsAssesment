library(terra)
library(caret)

#input unbiased historical climate folder
input_climHistorical <-  "F:/DATA_AUX1/sensitivityModels/1_data/processed/biased/sum/historical"
#input SPRACC folder
input_spracc <- "F:/DATA_AUX1/sensitivityModels/1_data/spracc"
#output study area folders for future climate
output_climFuture <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/biased/spracc"

#### GET DATA ####

hist.data <- list.files(input_climHistorical,full.names=T,recursive=T,pattern=".tif$")
spracc.data <- list.files(input_spracc,full.names=T,pattern=".tif$")
#check correlation
spracc.cor <- rast(spracc.data)
names(spracc.cor) <- c("t1","t2","t3","t4","t5")
spracc.cor <- cor(as.matrix(spracc.cor)[,-4],use="pairwise.complete.obs")
findCorrelation(spracc.cor,0.97)
spracc.cor
#remove correlated
spracc.data <- spracc.data[-4]
stats.spracc <- lapply(spracc.data,function(x){
  x.val <- values(rast(x)*365)
  x.val <- data.frame(nam=unlist(strsplit(basename(x),"_"))[8],
                      median=median(x.val,na.rm=T),
                      SD=sd(x.val,na.rm=T))
  
  return(x.val)
})
stats.spracc <- do.call("rbind.data.frame",stats.spracc)
stats.spracc <- stats.spracc[order(stats.spracc$median,decreasing = T),]
stats.spracc
#operate
for(i in 1:length(spracc.data)){
  #get type
  ssp.i <- unlist(strsplit(basename(spracc.data[i]),"_"))[8]
  if(ssp.i=="Type2"){
    ssp.i <- "ssp126"
  }else if(ssp.i=="Type5"){
    ssp.i <- "ssp245"
  }else if(ssp.i=="Type3"){
    ssp.i <- "ssp370"
  }else if(ssp.i=="Type1"){
    ssp.i <- "ssp585"
  }
  #create folder
  ssp.dir <- paste0(output_climFuture,"/",ssp.i)
  if(!dir.exists(ssp.dir)){
    dir.create(ssp.dir,recursive=T,showWarnings = F)
  }
  #iterate study areas
  for(j in 1:length(hist.data)){
    code.j <- basename(dirname(hist.data[j]))
    hist.j <- rast(hist.data[j])["prec"]
    #get annual prec
    spracc.j <- rast(spracc.data[i])*365
    spracc.j <- crop(spracc.j,hist.j)
    spracc.j <- resample(spracc.j,hist.j)
    spracc.j <- raster::mask(spracc.j,hist.j)
    names(spracc.j) <- "prec_sum"
    #operate difference
    spracc.j.diff <- spracc.j - hist.j
    names(spracc.j.diff) <- "diff"
    #create folder
    output.spracc <- paste0(ssp.dir,"/",code.j)
    if(!dir.exists(output.spracc)){
      dir.create(output.spracc,recursive=T,showWarnings = F)
    }
    #save raster
    j.rast <- rast(list(spracc.j,spracc.j.diff))
    out.file <- paste0(output.spracc,"/prec_SPRACC_",ssp.i,"_2021-2040.tif")
    writeRaster(j.rast,out.file,datatype="FLT4S",overwrite=T)
    #extract values
    j.ope.val <- values(j.rast)
    j.ope.val <- j.ope.val[!is.na(j.ope.val)]
    j.ope.val <- j.ope.val[!is.nan(j.ope.val)] 
    j.ope.val <- round(j.ope.val,1)
    j.diff.val <- values(spracc.j.diff)
    j.diff.val <- j.diff.val[!is.na(j.diff.val)]
    j.diff.val <- j.diff.val[!is.nan(j.diff.val)]
    j.diff.val <- round(j.diff.val,1)
    j.df <- data.frame(code=code.j,
                       param="prec",
                       model="SPRACC",
                       ssp=ssp.i,
                       step="2021-2040",
                       value=j.ope.val,
                       diff=j.diff.val)
    #save table
    out.file <- paste0(output.spracc,"/prec_SPRACC_",ssp.i,"_2021-2040.csv")
    write.csv(j.df,out.file,row.names=F)
  }
}