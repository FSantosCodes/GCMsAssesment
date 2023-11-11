library(terra)
library(xlsx)
library(reshape2)
library(ggplot2)
library(ggh4x)
library(sf)
library(Metrics)
library(lubridate)

#input historical (processed)
historical_sum <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/historical"
historical_monthly <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/monthly/historical"
#input future (raw) folder
future_raw <- c("F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp126",
                "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp245",
                "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp370",
                "F:/DATA_AUX1/sensitivityModels/1_data/raw/ssp585")
#input elevation
elev_file <- "F:/DATA_AUX1/sensitivityModels/1_data/elev/wc2.1_2.5m_elev.tif"
#input study areas
studyAreas_shp <- "F:/DATA_AUX1/sensitivityModels/1_data/shp/biogeographic_sectors_GWS.shp"
#output folder
output_folder <- "F:/DATA_AUX1/sensitivityModels/3_plots"

#### PREPARE ELEV ####

#read study areas
areas.shp <- vect(studyAreas_shp)
areas.shp <- areas.shp[order(areas.shp$CODE),]
#read elev
elev.ras <- rast(elev_file)
#cut and derive table
elev.tbl <- lapply(1:nrow(areas.shp),function(x){
  x.area <- areas.shp[x,]
  x.ras <- crop(elev.ras,ext(x.area))
  x.ras <- mask(x.ras,x.area)
  x.ras <- values(x.ras)
  x.ras <- x.ras[!is.na(x.ras)]
  x.ras <- x.ras[!is.nan(x.ras)]
  x <- data.frame(code=x.area$CODE,elev=x.ras)
})
elev.tbl <- do.call("rbind.data.frame",elev.tbl)
rm(elev.ras)

#### PROCESS SUM ####

#get sum tables
sum.tbl <- list()
for(i in 1:nrow(areas.shp)){
  hist.tbl <- list.dirs(historical_sum)[-1]
  hist.tbl <- list.files(hist.tbl[i],full.names=T,pattern="csv")
  hist.tbl <- read.csv(hist.tbl)
  sum.tbl[[i]] <- hist.tbl
}
sum.tbl <- do.call("rbind.data.frame",sum.tbl)

#### PROCESS MONTHLY ####

#get monthly tables
monthly.tbl <- list()
for(i in 1:nrow(areas.shp)){
  hist.tbl <- list.dirs(historical_monthly)[-1]
  hist.tbl <- list.files(hist.tbl[i],full.names=T,pattern="csv")
  hist.tbl <- read.csv(hist.tbl)
  monthly.tbl[[i]] <- hist.tbl
}
monthly.tbl <- do.call("rbind.data.frame",monthly.tbl)
monthly.tbl$model <- NULL
rm(hist.tbl)

#### DERIVE OVERALL ####

#prepare overall
sum.tbl$elev <- elev.tbl$elev
sum.tbl$model <- NULL
overall.tbl <- split(sum.tbl,sum.tbl$code)
overall.tbl <- lapply(overall.tbl,function(x){
  x.tbl <- round(as.data.frame((sapply(x[,-1],summary))),1)
  x.tbl$stat <- rownames(x.tbl)
  x.tbl$code <- unique(x$code)
  rownames(x.tbl) <- NULL
  x.tbl$npix <- nrow(x)
  x.tbl$km2 <- expanse(areas.shp[areas.shp$CODE==unique(x$code),],unit="km")
  x.tbl <- x.tbl[,c(6,8,7,5,1:4)]
  x.sd <- x.tbl[1,]
  x.sd$stat <- "SD"
  x.sd$prec <- round(sd(x$prec),1)
  x.sd$tmax_mn <- round(sd(x$tmax_mn),1)
  x.sd$tmin_mn <- round(sd(x$tmin_mn),1)
  x.sd$elev <- round(sd(x$elev),1)
  x.tbl <- rbind(x.tbl,x.sd)
  return(x.tbl)
})
overall.tbl <- do.call("rbind.data.frame",overall.tbl)
rownames(overall.tbl) <- NULL
#save
# out.file <- paste0(output_folder,"/annex1_studyAreaStats.xlsx")
# write.xlsx(overall.tbl,out.file,row.names = F,sheetName = "HistoricalStats")

#### PROCESS FUTURE ####

future.tbl <- list.files(future_raw,recursive=T,pattern=".tif$")
future.tbl <- gsub(".tif|wc2.1_2.5m_","",basename(future.tbl))
future.tbl <- gsub("prec_","",grep("prec_",future.tbl,value=T))
future.tbl <- strsplit(future.tbl,"_")
future.tbl <- data.frame(model=sapply(future.tbl,"[[",1),
                         SSP=sapply(future.tbl,"[[",2),
                         timeStep=sapply(future.tbl,"[[",3))
future.tbl <- split(future.tbl,future.tbl$model)
future.tbl <- lapply(future.tbl,function(x){
  x <- data.frame(model=unique(x$model),
                  nFiles=nrow(x),
                  nSSPs=length(unique(x$SSP)),
                  nTimeSteps=length(unique(x$timeStep)),
                  SSPs=gsub("ssp","",paste(unique(x$SSP),collapse=";")),
                  timeSteps=paste(unique(x$timeStep),collapse=";"))
})
future.tbl <- do.call("rbind.data.frame",future.tbl)
rownames(future.tbl) <- NULL
#save
# out.file <- paste0(output_folder,"/annex1_studyAreaStats.xlsx")
# write.xlsx(future.tbl,out.file,row.names = F,append =T,sheetName  = "FutureModels")


#### PLOT ELEV ####

elev.tbl$var <- "ELEVATION"
elev.outliers <- as.numeric(quantile(elev.tbl$elev,probs=c(0.01,0.99)))
elev.tbl <- elev.tbl[elev.tbl$elev>elev.outliers[1],]
elev.tbl <- elev.tbl[elev.tbl$elev<elev.outliers[2],]
q <- ggplot(elev.tbl,aes(x=code,y=elev,group=code)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,size=8,hjust = 1),
        axis.text.y = element_text(size=8), 
        panel.grid.minor.y = element_blank()) +
  geom_boxplot(outlier.size=0.85,outlier.shape = 20,outlier.alpha=0.1) +
  xlab("Study areas") +
  ylab("masl") +
  coord_cartesian(ylim=c(0,3000)) +
  facet_nested_wrap(var~.,scales="free",nrow=1)
out.file <- paste0(output_folder,"/plot2b_elevation.jpg")
# jpeg(out.file,width = 5, height = 5.4, units = "cm", res = 330)
# q
# dev.off()

#### PLOT MONTLY ####

#prepare data
prec.monthly <- reshape2::melt(monthly.tbl[,1:13],id.vars="code")
prec.monthly$month <- as.character(prec.monthly$variable)
prec.monthly$month <- sapply(strsplit(prec.monthly$month,"_"),"[[",2)
prec.monthly$month <- factor(prec.monthly$month,labels=month.abb)
prec.monthly$variable <- as.character(prec.monthly$variable)
prec.monthly$variable <- factor(sapply(strsplit(prec.monthly$variable,"_"),"[[",1))
#get median line
prec.med <- split(prec.monthly,prec.monthly$code)
prec.med <- lapply(prec.med,function(x){
  x.data <- split(x,x$month)
  x.data <- lapply(x.data,function(y){
    return(median(y$value))
  })
  x.data <- data.frame(code=unique(x$code),
                       month=names(x.data),
                       value=as.numeric(x.data))
  return(x.data)
})
prec.med <- do.call("rbind.data.frame",prec.med)
rownames(prec.med) <- NULL
#sort months
prec.monthly$month <- factor(prec.monthly$month,
                             levels=c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug"))
#plot
p <- ggplot(prec.monthly,aes(x=month,y=value,group=month)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,size=8,hjust = 1),
        axis.text.y = element_text(size=8), 
        panel.grid.minor.y = element_blank()) +
  geom_boxplot(outlier.size=0.85,outlier.shape = 20,outlier.alpha=0.1) +
  geom_smooth(data=prec.med,aes(x=month,y=value,group=code),
              method="gam",span=10000,se=F,linewidth=0.5,color="red") +
  xlab("Months") +
  ylab("Monthly precipitation (mm)") +
  facet_wrap(code~.,scales="free")
p
#save
# out.file <- paste0(output_folder,"/plot2_rainfallHistorical_v2.jpg")
# jpeg(out.file,width = 17, height = 10, units = "cm", res = 330)
# p
# dev.off()

#### CHECK COUNTRIES ####

modelling.institutes <- read.xlsx("F:/DATA_AUX1/sensitivityModels/3_plots/annex1_studyAreaStats.xlsx",3)
modelling.institutes[modelling.institutes=="-"] <- NA
#how many contries?
table(modelling.institutes$COUNTRY)
4+2+2+1+1+2 #europe
2+3 #asia
3+2 #north america
2 #oceania


#what are the nominal resolution?
table(as.numeric(modelling.institutes$NRE))
#how many components?
table(apply(modelling.institutes[,3:11],1,function(x){
  x <- length(x[!is.na(x)])
}))

#### CHECK TEMPERATURES ####

temp.tbl <- list.files("F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/historical",full.names=T,pattern=".csv",recursive=T)
temp.tbl <- lapply(temp.tbl,function(x){
  x.data <- read.csv(x)
  x.data <- x.data[complete.cases(x.data),]
  x <- round(c(mean(x.data$tmin_mn),mean(x.data$tmax_mn)),2)
})
temp.tbl



#### VALIDATE HISTORICAL ####

#annual prec
sum.prec <- rast("F:/DATA_AUX1/sensitivityModels/1_data/hist/histAvg_context.tif")
inamhi.est <- vect("F:/DATA_AUX1/sensitivityModels/1_data/inamhi/Isoyeta 1981-2010/ISOYETAS_1981-2010_GWS.shp")
inamhi.est$hist <- extract(sum.prec,inamhi.est)[,2]
cor(inamhi.est$ACUMULADO,inamhi.est$hist,method="spearman")
rmse(inamhi.est$ACUMULADO,inamhi.est$hist)
#check by study areas
study.areas <- vect(studyAreas_shp)
study.areas <- intersect(study.areas,inamhi.est)
study.areas$hist <- extract(sum.prec,study.areas)[,2]
study.areas <- as.data.frame(study.areas)
study.areas <- split(study.areas,study.areas$CODE)
lapply(study.areas,function(x){
  cor(x$ACUMULADO,x$hist,method="spearman")
})
lapply(study.areas,function(x){
  rmse(x$ACUMULADO,x$hist)
})
lapply(study.areas,function(x){
  nrow(x)
})
#monthly prec
inamhi.mon <- vect("F:/DATA_AUX1/sensitivityModels/1_data/inamhi/Isoyeta 1981-2010/ISOYETAS_1981-2010_meses_GWS.shp")
inamhi.mon <- inamhi.mon[inamhi.mon$CODIGO %in% inamhi.est$codigo,]
mon.prec <- list.files("F:/DATA_AUX1/sensitivityModels/1_data/raw/historical",full.names=T,pattern=".tif$")
mon.prec <- grep("prec",mon.prec,value=T)
mon.prec <- rast(mon.prec)
names(mon.prec) <- paste0("prec_",month.abb)
mon.prec <- extract(mon.prec,inamhi.mon)[,-1]
inamhi.mon <- as.data.frame(inamhi.mon)
inamhi.mon <- inamhi.mon[,7:18]
names(inamhi.mon) <- paste0("ref_",month.abb)
metr.df <- list()
for(i in 1:ncol(inamhi.mon)){
  cor.i <- cor(inamhi.mon[,i],mon.prec[,i],method="spearman")
  rmse.i <- rmse(inamhi.mon[,i],mon.prec[,i])
  month.i <- gsub("prec_","",names(inamhi.mon)[i])
  metr.df[[i]] <- data.frame(month=month.i,
                             cor=cor.i,
                             rmse=rmse.i)
}
metr.df <- do.call("rbind.data.frame",metr.df)
metr.df

#### VALIDATE GALAPAGOS ####

gal.files <- list.files("F:/DATA_AUX1/sensitivityModels/1_data/inamhi/galapagos",
                        full.names=T,pattern=".csv")
gal.data <- lapply(gal.files,function(x){
  x.data <- read.csv(x)
  x.data <- x.data[,c(1,7)]
  x.data$observation_date <- ymd(x.data$observation_date)
  x.data <- x.data[x.data$observation_date<=ymd("2000-12-31"),]
  x.data <- x.data[x.data$observation_date>=ymd("1980-01-01"),]
  x.data$yrs <- sapply(strsplit(as.character(x.data$observation_date),"-"),"[[",1)
  x.data$mth <- sapply(strsplit(as.character(x.data$observation_date),"-"),"[[",2)
  x.data$day <- sapply(strsplit(as.character(x.data$observation_date),"-"),"[[",3)
  x.data <- split(x.data,x.data$yrs)
  x.data <- lapply(x.data,function(y){
    y.data <- split(y,y$mth)
    y.data <- lapply(y.data,function(z){
      sum(z$precipitation)
    })
    y.data <- sum(unlist(y.data)) / length(unlist(y.data))
  })
  x.data <- sum(unlist(x.data)) / length(unlist(x.data))
  x.name <- gsub(".csv","",basename(x))
  x.data <- data.frame(name=x.name,prec=x.data)
  return(x.data)
})
gal.data <- do.call("rbind.data.frame",gal.data)
gal.data$lat  <- c(-0.692384,-0.743708)
gal.data$lon  <- c(-90.328160,-90.302647)

gal.vec <- vect(gal.data,crs=crs("epsg:4326"))
writeVector(gal.vec,"F:/DATA_AUX1/sensitivityModels/1_data/inamhi/galapagos/gal_sta.shp")
sum.prec <- rast("F:/DATA_AUX1/sensitivityModels/1_data/hist/histAvg_context.tif")

gal.prec <- extract(sum.prec,gal.vec)[,-1]
gal.data$prec_sta <- gal.prec

cor(gal.data$prec,gal.data$prec_sta,method="spearman")
rmse(gal.data$prec,gal.data$prec_sta)


#### CHECK PROCESSING TIME ####

process.time <- list.files("F:/DATA_AUX1/sensitivityModels/1_data/processed/sum",full.names=T,recursive=T,pattern="processingReport")
process.time <- do.call("rbind.data.frame",lapply(process.time,read.csv))
summary(process.time$time)





