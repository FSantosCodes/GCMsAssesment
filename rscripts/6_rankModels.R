library(data.table)
library(ggplot2)
library(reshape2)
library(viridis)
library(xlsx)
library(ggh4x)
library(RColorBrewer)
library(ggstats)

#input folders
climFuture_ssp126 <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/ssp126"
climFuture_ssp245 <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/ssp245"
climFuture_ssp370 <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/ssp370"
climFuture_ssp585 <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/sum/ssp585"

#output folder
output_folder <- "F:/DATA_AUX1/sensitivityModels/3_plots"

#### PREPARE DATA ####

#read function
sspRead <- function(inputFolder,param="value"){
  #read files
  x.folders <- list.dirs(inputFolder,full.names=T)[-1]
  #prec
  x.prec <- lapply(x.folders,function(x){
    x.files <- list.files(x,full.names=T,pattern=".csv")
    x.files <- grep("prec_",x.files,value=T)
    x.models <- sapply(strsplit(basename(x.files),"_"),"[[",2)
    x.models <- split(x.files,x.models)
    x.csv <- lapply(x.models,function(y){
      y.data <- lapply(y,fread)
      y.data <- as.data.frame(do.call("rbind.data.frame",y.data))
      return(y.data)
    })
    x.csv <- do.call("rbind.data.frame",x.csv)
    rownames(x.csv) <- NULL
    return(x.csv)
  })
  x.prec <- do.call("rbind.data.frame",x.prec)
  return(x.prec)
}
#get data
ssp126.data <- sspRead(climFuture_ssp126,param="diff")
ssp245.data <- sspRead(climFuture_ssp245,param="diff")
ssp370.data <- sspRead(climFuture_ssp370,param="diff")
ssp585.data <- sspRead(climFuture_ssp585,param="diff")

#### PRECIPITATION RANKING ####

#merge prec
prec.data <- list(ssp126.data,ssp245.data,ssp370.data,ssp585.data)
prec.data <- do.call("rbind.data.frame",prec.data)
#prepare
names(prec.data)[4] <- "SSP"
prec.data.melt <- melt(prec.data[,c("code","model","SSP","diff")],id.vars=c("code","SSP","model"))
prec.data.melt$variable <- NULL
names(prec.data.melt)[3] <- "variable"
prec.data.melt$SSP <- factor(prec.data.melt$SSP,labels=c("1-2.6","2-4.5","3-7.0","5-8.5"))
#prepare global ranking (with median)
prec.model.ranking <- reorder(prec.data.melt$variable,prec.data.melt$value,FUN=median)
prec.model.ranking <- attr(prec.model.ranking,"scores")
prec.model.ranking <- data.frame(model=names(prec.model.ranking),global=prec.model.ranking)
rownames(prec.model.ranking) <- NULL
prec.model.ranking <- prec.model.ranking[order(prec.model.ranking$model),]
#prepare ranking by areas (with median)
prec.model.areas <- split(prec.data.melt,prec.data.melt$code)
prec.model.areas <- lapply(prec.model.areas,function(x){
  x.ranking <- reorder(x$variable,x$value,FUN=median)
  x.ranking <- attr(x.ranking,"scores")
  x.ranking <- data.frame(model=names(x.ranking),area=x.ranking)
  rownames(x.ranking) <- NULL
  names(x.ranking)[2] <- unique(x$code)
  x.ranking <- x.ranking[order(x.ranking$model),]
  x.ranking$model <- NULL
  return(x.ranking)
})
prec.model.areas <- do.call("cbind.data.frame",prec.model.areas)
prec.model.ranking <- cbind(prec.model.ranking,prec.model.areas)
model.rank <- prec.model.ranking[order(prec.model.ranking$global,decreasing =T),]
#save
# out.file <- paste0(output_folder,"/annex2_ranking.xlsx")
# write.xlsx(prec.model.ranking,out.file,sheetName="StudyAreas-medians",row.names=F)

#### SSP ORDER ####

rownames(prec.data.melt) <- NULL
ssp.df <- split(prec.data.melt,prec.data.melt$variable)
ssp.df <- lapply(ssp.df,function(x){
  x.data <- split(x,x$SSP)
  x.ssp <- lapply(x.data,function(y){
    y.ssp <- as.character(unique(y$SSP))
    if(length(y.ssp)==0){
      y <- NA
    }else{
      y.model <- as.character(unique(y$variable))
      y.median <- (median(y$value,na.rm=T))
      y <- data.frame(model=y.model,SSP=y.ssp,median=y.median)
    }
    return(y)
  })
  x.ssp <- x.ssp[!sapply(lapply(x.ssp,is.na),all)]
  x.ssp <- do.call("rbind.data.frame",x.ssp)
  rownames(x.ssp) <- NULL
  x.ssp <- x.ssp[order(x.ssp$median),]
  return(x.ssp)
})
#get min
ssp.min <- lapply(ssp.df,function(x){
  x[which.min(x$median),]
})
ssp.min <- do.call("rbind.data.frame",ssp.min)
rownames(ssp.min) <- NULL
table(ssp.min$SSP)
#get max
ssp.max <- lapply(ssp.df,function(x){
  x[which.max(x$median),]
})
ssp.max <- do.call("rbind.data.frame",ssp.max)
rownames(ssp.max) <- NULL
table(ssp.max$SSP)
#store data
ssp.df <- do.call("rbind.data.frame",ssp.df)
rownames(ssp.df) <- NULL
#save
# out.file <- paste0(output_folder,"/annex2_ranking.xlsx")
# write.xlsx(ssp.df,out.file,sheetName="SSP-medians",row.names=F,append=T)

#### PLOT ####

#remove 1%
prec.data.melt <- split(prec.data.melt,prec.data.melt$code)
prec.data.melt <- lapply(prec.data.melt,function(x){
  x <- x[!is.na(x$value),]
  x.outliers <- as.numeric(quantile(x$value,probs=c(0.01,0.99)))
  x <- x[x$value>x.outliers[1] & x$value<x.outliers[2],]
  return(x)
})
prec.data.melt <- do.call("rbind.data.frame",prec.data.melt)
#red lines
prec.med <- split(prec.data.melt,prec.data.melt$variable)
prec.med <- lapply(prec.med,function(x){
  x.med <- split(x,x$code)
  x.med <- lapply(x.med,function(y){
    y.med <- median(y$value,na.rm=T)
    y.med <- data.frame(code=unique(y$code),
                        variable=unique(y$variable),
                        value=y.med)
    return(y.med)
  })
  x.med <- do.call("rbind.data.frame",x.med)
  rownames(x.med) <- NULL
  return(x.med)
})
prec.med <- do.call("rbind.data.frame",prec.med)
rownames(prec.med) <- NULL
#order
prec.data.melt$variable <- factor(prec.data.melt$variable,rev(model.rank$model))
prec.med$variable <- factor(prec.med$variable,rev(model.rank$model))
#plot
p <- ggplot(prec.data.melt,aes(x=variable,y=value,fill=SSP,group=interaction(SSP,variable))) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45,size=8,hjust = 1),
        axis.text.y = element_text(size=8),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  geom_boxplot(size=0.1,outlier.size=0.05,outlier.shape = 20,outlier.alpha=0.1) +
  geom_hline(yintercept=0,linetype="dashed") +
  scale_fill_manual(values=adjustcolor(rev(brewer.pal(n=4,"RdYlGn")),0.8)) +
  scale_y_continuous(n.breaks=6) +
  scale_x_discrete(expand = c(0.026, 0.026)) +
  xlab("Models") +
  ylab("Annual precipitation anomalies (mm)") +
  facet_wrap(code~.,scales="free_y",ncol=1,strip.position="right") +
  geom_stripped_cols(color=NA)

#save
# out.file <- paste0(output_folder,"/plot4_boxplotSSP_v6.jpg")
# jpeg(out.file,width = 17, height = 20, units = "cm", res = 330)
# p
# dev.off()

