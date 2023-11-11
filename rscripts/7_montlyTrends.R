library(data.table)
library(ggplot2)
library(dtwclust)
library(cluster)
library(factoextra)
library(ggh4x)
library(viridis)
library(xlsx)
library(tictoc)
library(DescTools)
library(ggpubr)

#input folders
climFuture_ssp126 <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/monthly/ssp126"
climFuture_ssp245 <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/monthly/ssp245"
climFuture_ssp370 <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/monthly/ssp370"
climFuture_ssp585 <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/monthly/ssp585"
#historical folder
historical_monthly <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/monthly/historical"
#models ranking
model_rank <- "F:/DATA_AUX1/sensitivityModels/3_plots/annex2_ranking.xlsx"
#output folder
output_folder <- "F:/DATA_AUX1/sensitivityModels/3_plots"

#### PREPARE DATA ####

#read function
sspRead <- function(inputFolder,param="months"){
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
      y.model <- unique(y.data$model)
      if(param=="months"){
        y.data <- y.data[,c(1:17)]
      }else{
        y.data <- y.data[,c(1:5,18:29)]
      }
      names(y.data)[6:17] <- month.abb
      #get median by time step
      y.data.med <- split(y.data[6:17],y.data$step)
      y.data.med <- sapply(y.data.med,function(z){
        z <- round(sapply(z,median))
      })
      y.data.med <- as.data.frame(t(as.matrix(y.data.med)))
      y.data.med$step <- rownames(y.data.med)
      y.data.med <- y.data.med[,c(13,1:12)]
      rownames(y.data.med) <- NULL
      #merge with id
      y.data <- y.data[1:4,1:4]
      y.data <- cbind(y.data,y.data.med)
      return(y.data)
    })
    x.csv <- do.call("rbind.data.frame",x.csv)
    rownames(x.csv) <- NULL
    return(x.csv)
  })
  x.prec <- do.call("rbind.data.frame",x.prec)
  #close
  return(x.prec)
}
#get data
ssp126.data <- sspRead(climFuture_ssp126,param="months")
ssp245.data <- sspRead(climFuture_ssp245,param="months")
ssp370.data <- sspRead(climFuture_ssp370,param="months")
ssp585.data <- sspRead(climFuture_ssp585,param="months")
#merge prec
prec.data <- list(ssp126.data,ssp245.data,ssp370.data,ssp585.data)
prec.data <- do.call("rbind.data.frame",prec.data)
names(prec.data)[4] <- "SSP"
prec.data$SSP <- gsub("ssp","",prec.data$SSP)
rm(ssp126.data,ssp245.data,ssp370.data,ssp585.data)

#### CLUSTER TIME-SERIES ####

#without transform data
prec.raw <- prec.data[,6:17]
#normalize data
ts.data <- zscore(prec.data[,6:17])
#evaluate number of clusters
p.wws <- fviz_nbclust(ts.data, kmeans, method = "wss")
p.sil <- fviz_nbclust(ts.data, kmeans, method = "silhouette")
p.gst <- fviz_gap_stat(clusGap(ts.data,FUNcluster = kmeans, K.max = 15))
#number of clusters
nclusters <- 8
#perform clustering - dtw
ts.dtw <- tsclust(ts.data, k = nclusters,
                  distance = "dtw_basic", centroid = "dba",
                  trace = TRUE, seed = 8,
                  norm = "L2", window.size = 2L,
                  args = tsclust_args(cent = list(trace = TRUE)))
#perform clustering - dtw_lb
ts.dtwlb <- tsclust(ts.data, k = nclusters,
                    distance = "dtw_lb", centroid = "dba",
                    trace = TRUE, seed = 8,
                    norm = "L2", window.size = 2L,
                    control = partitional_control(pam.precompute = FALSE),
                    args = tsclust_args(cent = list(trace = TRUE)))
#perform clustering - sbd
ts.sbd <- tsclust(ts.data, k = nclusters,
                    distance = "sbd", centroid = "dba",
                    seed = 8, trace = TRUE)
#perform clustering - tp
ts.tp <- tsclust(ts.data, k = nclusters, type = "t",
                 seed = 8, trace = TRUE,
                 control = tadpole_control(dc = 1.5,
                                           window.size = 2L))
#evaluation
eval <- sapply(list(DTW = ts.dtw, DTW_LB = ts.dtwlb, kShape = ts.sbd, TADPole = ts.tp),
                cvi, b = 1:nrow(ts.data), type = "VI")
eval
#plot best “dendrogram”, “series”, “centroids”, “sc”
plot(ts.dtw, type = "series")
#perform labelling
prec.data$cluster <- ts.dtw@cluster
prec.data <- prec.data[,c("code","param","model","SSP","step","cluster",month.abb)]

#### PLOT PROTOTYPES ####

#get series
prec.ser <- prec.data
prec.ser[,month.abb] <- ts.data
prec.ser$id <- 1:nrow(prec.ser)
prec.ser <- reshape2::melt(prec.ser,id.vars=c("cluster","id"),measure.vars=month.abb)
names(prec.ser)[3] <- "months"
#get order
prec.clust.ord <- sapply(split(prec.ser$value,prec.ser$cluster),median)
prec.clust.ord <- data.frame(cluster=names(prec.clust.ord),medVal=prec.clust.ord)
prec.clust.ord <- prec.clust.ord[order(prec.clust.ord$medVal),]
#prepare group
prec.ser$group <- NA
prec.ser$group <- ifelse(prec.ser$cluster %in% c(8,3,6),"A",prec.ser$group)
prec.ser$group <- ifelse(prec.ser$cluster %in% c(1,2),"B",prec.ser$group)
prec.ser$group <- ifelse(prec.ser$cluster %in% c(4,5,7),"C",prec.ser$group)
prec.ser$group <- factor(prec.ser$group)
#reclass clusters
prec.ser$cluster <- factor(prec.ser$cluster,
                           levels=c(8,3,6,
                                    1,2,
                                    4,5,7),labels=c(1:8))
#set months
prec.ser$months <- factor(prec.ser$months,
                          levels=c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug"))
#plot
p <- ggplot(prec.ser,aes(x=months,y=value,color=cluster,group=cluster)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45,size=8,hjust = 1),
        axis.text.y = element_text(size=8), 
        panel.grid.minor.y = element_blank()) +
  geom_line(aes(x=months,y=value,group=interaction(id,cluster)),inherit.aes = F,color="gray60",alpha=0.1) +
  geom_smooth(fill=NA) +
  scale_color_viridis(discrete=T,option="H",direction=-1) +
  geom_hline(yintercept = 0,linetype="dashed",color="black",size=0.5) +
  facet_wrap(group~.,nrow=3) +
  xlab("Months") +
  ylab("Monthly precipitation (z-scores)") +
  guides(color = guide_legend("Clusters Groups A B C",ncol=3,override.aes = list(linewidth=2)))
p
# #save
# out.file <- paste0(output_folder,"/plot3_timeSeriesPrototypes2.jpg")
# jpeg(out.file,width = 12, height = 12, units = "cm", res = 330)
# p
# dev.off()

#### REVIEW MAXIMUM & MINIMUM CLUSTERS ####

#get maximum & minimum
clus.max <- split(prec.ser,prec.ser$cluster)
clus.max <- lapply(clus.max,function(x){
  x[which.max(x$value),]
})
clus.max <- do.call("rbind.data.frame",clus.max)
clus.max
clus.min <- split(prec.ser,prec.ser$cluster)
clus.min <- lapply(clus.min,function(x){
  x[which.min(x$value),]
})
clus.min <- do.call("rbind.data.frame",clus.min)
clus.min
#calculate AUC
auc.prec <- split(prec.ser,prec.ser$cluster)
auc.prec <- lapply(auc.prec,function(x){
  x.val <- as.numeric(x$months)
  y.val <- x$value
  AUC(x=x.val,y=y.val,absolutearea=F,na.rm=T,ties=mean)
})
auc.prec

#### PLOT MODEL CLUSTERS ####

#time-series prototypes frequencies
model.rank <- read.xlsx(model_rank,1)
model.rank <- model.rank[order(model.rank$order,decreasing=T),]
model.rank$order <- NULL
prec.clust <- as.data.frame(table(prec.data$code,prec.data$model,prec.data$cluster,prec.data$SSP))
names(prec.clust) <- c("code","model","cluster","SSP","freq")
prec.clust$model <- factor(as.character(prec.clust$model),model.rank$model)
#get groups
prec.clust$group <- NA
prec.clust$group <- ifelse(prec.clust$cluster %in% c(8,3,6),"A",prec.clust$group)
prec.clust$group <- ifelse(prec.clust$cluster %in% c(1,2),"B",prec.clust$group)
prec.clust$group <- ifelse(prec.clust$cluster %in% c(4,5,7),"C",prec.clust$group)
prec.clust$group <- factor(prec.clust$group)
#reclass clusters
prec.clust$cluster <- factor(prec.clust$cluster,levels=c(8,3,6,
                                                         1,2,
                                                         4,5,7),labels=1:8)
levels(prec.clust$SSP) <- c("1-2.6","2-4.5","3-7.0","5-8.5")
#plot
q <- ggplot(prec.clust[prec.clust$freq!=0,],aes(x=model,y=freq,fill=cluster)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=8),
        strip.text.x = element_text(size = 8,angle=90),
        panel.spacing = unit(0.2, "lines"),
        panel.ontop = TRUE,
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(color="white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  geom_bar(stat="identity") +
  scale_fill_viridis(discrete=T,option="H",direction=-1) +
  coord_flip() +
  facet_nested_wrap(code~SSP,nrow=1) +
  ylab("Time steps") +
  xlab("Models") +
  guides(fill = guide_legend("Clusters \nGroups A B C",ncol=3))
q
#save
# out.file <- paste0(output_folder,"/plot5_modelsPrototypes2.jpg")
# jpeg(out.file,width = 17, height = 20, units = "cm", res = 330)
# q
# dev.off()

#### EVALUATE MODELS BY STUDY AREA ####

#stat funcion
stats.fun <- function(x){
  x.clust <- prec.clust[prec.clust$code==x,]
  x.clust$model <- as.character(x.clust$model)
  x.clust$cluster <- as.character(x.clust$cluster)
  x.tb <- as.data.frame(table(x.clust$cluster[x.clust$freq!=0]))
  x.tb <- x.tb[order(x.tb$Freq),]
  x.tb$per <- round((x.tb$Freq*100)/sum(x.tb$Freq),1)
  return(x.tb)
}
#derive global
glob.tb <- as.data.frame(table(prec.clust$cluster[prec.clust$freq!=0]))
glob.tb <- glob.tb[order(glob.tb$Freq),]
glob.tb$per <- round((glob.tb$Freq*100)/sum(glob.tb$Freq),1)
glob.tb
#derive some AMZ stats
amz.tb <- stats.fun("AMZ")
amz.tb
#derive some AND stats
and.tb <- stats.fun("AND")
and.tb
#derive some CHO stats
cho.tb <- stats.fun("CHO")
cho.tb
#derive some EQP stats
eqp.tb <- stats.fun("EQP")
eqp.tb
#derive some EQP stats
gal.tb <- stats.fun("GAL")
gal.tb

#### COMPARE WITH HISTORICAL ####

#get historical
hist.data <- list.files(historical_monthly,recursive=T,full.names=T,pattern=".csv")
hist.data <- lapply(hist.data,function(x){
  x <- read.csv(x)[,c(1,3:14)]
  x.data <- as.data.frame(lapply(x[,2:13],median))
  names(x.data) <- month.abb
  x <- cbind(x[1,1,drop=F],x.data)
})
hist.data <- do.call("rbind.data.frame",hist.data)
#prepare clusters
clus.data <- prec.data[,c(1,6:18)]
clus.data$cluster <- factor(clus.data$cluster,
                           levels=c(8,3,6,
                                    1,2,
                                    4,5,7),labels=c(1:8))
clus.data <- split(clus.data,clus.data$code)
clus.data <- lapply(clus.data,function(x){
  x <- split(x,x$cluster)
  x <- lapply(x,function(y){
    y.data <- as.data.frame(lapply(y[,3:14],median))
    names(y.data) <- month.abb
    y <- cbind(y[1,1:2,drop=F],y.data)
  })
  x <- x[!is.na(sapply(x,"[[",1))]
  x <- do.call("rbind.data.frame",x)
})
clus.data <- do.call("rbind.data.frame",clus.data)
#compute dtw
compare.ls <- list()
for(i in 1:nrow(hist.data)){
  hist.i <- as.numeric(hist.data[i,2:13])
  clus.i <- split(clus.data,clus.data$code)[[i]]
  rownames(clus.i) <- NULL
  dtw.data <- list()
  for(j in 1:nrow(clus.i)){
    clus.j <- as.numeric(clus.i[j,3:14])
    dtw.j <- dtw(clus.j,hist.i)#,step.patterns = symmetricP1)
    warping.path <- dtw.j$index1
    distance <- dtw.j$distance
    dtw.j <- data.frame(code=as.character(clus.i$code[j]),
                        cluster=as.character(clus.i$cluster[j]),
                        ts1=clus.j[warping.path],
                        hist=hist.i[warping.path],
                        diff=clus.j[warping.path] - hist.i[warping.path],
                        wp=warping.path,
                        dist=dtw.j$distance)
    dtw.data[[j]] <- dtw.j
  }
  dtw.data <- do.call("rbind.data.frame",dtw.data)
  compare.ls[[i]] <- dtw.data
}
#difference plot
compare.ls <- do.call("rbind.data.frame",compare.ls)
#highlight most frequent
p.diff <- reshape2::melt(compare.ls[,c(1,2,5,6)],id.vars=c("code","cluster","wp"))
p.diff$freq <- "no"
p.diff[p.diff$code=="AMZ" & p.diff$cluster=="8","freq"] <- "yes"
p.diff[p.diff$code=="AND" & p.diff$cluster=="4","freq"] <- "yes"
p.diff[p.diff$code=="CHO" & p.diff$cluster=="3","freq"] <- "yes"
p.diff[p.diff$code=="EQP" & p.diff$cluster=="1","freq"] <- "yes"
p.diff[p.diff$code=="GAL" & p.diff$cluster=="2","freq"] <- "yes"
p.diff$wp <- factor(p.diff$wp,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
p.diff$wp <- factor(p.diff$wp,levels=c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug"))
p.diff$variable <- "Diference"

p <- ggplot(p.diff,aes(x=wp,y=value,group=cluster,color=cluster,linewidth=freq,alpha=freq)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45,size=8,hjust = 1),
        axis.text.y = element_text(size=8), 
        panel.grid.minor.y = element_blank()) +
  geom_line() +
  geom_hline(yintercept = 0,linetype="dashed") +
  scale_linewidth_manual(values=c(0.8,2)) +
  scale_alpha_manual(values=c(1,0.35)) +
  scale_color_manual(values=viridis(8, option = "H",direction=-1)) +
  geom_line(linewidth=0.8,alpha=1) +
  facet_nested_wrap(code~.,ncol=3,strip.position="top",scales="free") + #,scales="free_y"
  ylab("Monthly precipitation anomalies (mm)") +
  xlab("Months")
p

# out.file <- paste0(output_folder,"/plot6_clusterDistances3.jpg")
# jpeg(out.file,width = 17, height = 10, units = "cm", res = 330)
# p
# dev.off()
#add distances
q.dist <- split(compare.ls,compare.ls$code)
q.dist <- lapply(q.dist,function(x){
  x.split <- split(x,x$cluster)
  x.split <- lapply(x.split,function(y){
    y <- y[1,c("code","cluster","dist")]
  })
  x <- do.call("rbind.data.frame",x.split)
})
q.dist <- do.call("rbind.data.frame",q.dist)
rownames(q.dist) <- NULL
q.dist$cluster <- factor(q.dist$cluster)
q.dist$var <- "DISTANCE"
q <- ggplot(q.dist,aes(x=code,y=dist,fill=cluster)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45,size=8,hjust = 1),
        axis.text.y = element_text(size=8), 
        panel.grid.minor.y = element_blank()) +
  geom_bar(stat="identity",color="black",position=position_dodge(width = 0.8, preserve = "single"),width=0.8,linewidth=0.4) +
  scale_fill_manual(values=viridis(8, option = "H",direction=-1)) + 
  xlab("Study areas") +
  ylab("mm") +
  facet_nested_wrap(var~.,scales="free",nrow=1)
q
# out.file <- paste0(output_folder,"/plot6_clusterDistances2.jpg")
# jpeg(out.file,width = 5, height = 5.4, units = "cm", res = 330)
# q
# dev.off()
#derive stats
compare.ls$wp <- factor(compare.ls$wp,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
compare.ls$wp <- factor(compare.ls$wp,levels=c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug"))
compare.ls <- split(compare.ls,compare.ls$code)
#amz
compare.amz <- compare.ls[[1]]
compare.amz <- compare.amz[compare.amz$cluster %in% c(6,8),]
unique(compare.amz$dist)
compare.amz <- compare.amz[compare.amz$cluster==8,]
ggplot(compare.amz,aes(x=wp,y=diff)) +
  geom_bar(stat="identity")
compare.amz[compare.amz$wp %in% c("Mar","Apr","May","Jun"),]
compare.amz[!compare.amz$wp %in% c("Mar","Apr","May","Jun"),]
#and
compare.and <- compare.ls[[2]]
compare.and <- compare.and[compare.and$cluster %in% c(4,5),]
unique(compare.and$dist)
compare.and <- compare.and[compare.and$cluster==4,]
ggplot(compare.and,aes(x=wp,y=diff)) +
  geom_bar(stat="identity")
compare.and[compare.and$wp %in% c("Nov","Dec","Jan","Feb","Mar","Apr"),]
compare.and[!compare.and$wp %in% c("Nov","Dec","Jan","Feb","Mar","Apr"),]
#cho
compare.cho <- compare.ls[[3]]
compare.cho <- compare.cho[compare.cho$cluster %in% 3,]
unique(compare.cho$dist)
ggplot(compare.cho,aes(x=wp,y=diff)) +
  geom_bar(stat="identity")
compare.cho[compare.cho$wp %in% c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul"),]
compare.cho[!compare.cho$wp %in% c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul"),]
#eqp
compare.eqp <- compare.ls[[4]]
compare.eqp <- compare.eqp[compare.eqp$cluster %in% 1,]
unique(compare.eqp$dist)
compare.eqp <- compare.eqp[compare.eqp$cluster==1,]
ggplot(compare.eqp,aes(x=wp,y=diff)) +
  geom_bar(stat="identity")
compare.eqp[compare.eqp$wp %in% c("Dec","Jan","Feb","Mar","Apr","May","Jun"),]
compare.eqp[!compare.eqp$wp %in% c("Dec","Jan","Feb","Mar","Apr","May","Jun"),]
#gal
compare.gal <- compare.ls[[5]]
compare.gal <- compare.gal[compare.gal$cluster %in% 2,]
unique(compare.gal$dist)
compare.gal <- compare.gal[compare.gal$cluster==2,]
ggplot(compare.gal,aes(x=wp,y=diff)) +
  geom_bar(stat="identity")
compare.gal[compare.gal$wp %in% c("Dec","Jan","Feb","Mar","Apr","May","Jun"),]
#for discussion
compare.eqp[which.max(compare.eqp$diff),]
(16*100)/239
(3*100)/239

compare.gal[which.max(compare.gal$diff),]
(14*100)/58
(2.5*100)/58


#prepare data
# compare.ls <- do.call("rbind.data.frame",compare.ls)
# p.ts1 <- reshape2::melt(compare.ls[,c(1,2,3,6)],id.vars=c("code","cluster","wp"))
# p.ts1$wp <- factor(p.ts1$wp,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# p.ts1$wp <- factor(p.ts1$wp,levels=c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug"))
# p.ts1$variable <- "GCM"
# p.hist <- reshape2::melt(compare.ls[,c(1,2,4,6)],id.vars=c("code","cluster","wp"))
# p.hist$wp <- factor(p.hist$wp,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# p.hist$wp <- factor(p.hist$wp,levels=c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug"))
# p.hist$variable <- "Historical"
#do plot
# p <- ggplot(p.ts1,aes(x=wp,y=value,group=interaction(variable,cluster),color=cluster)) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle=45,size=8,hjust = 1),
#         axis.text.y = element_text(size=8), 
#         panel.grid.minor.y = element_blank()) +
#   geom_area(data=p.hist,color=NA,fill="gray70",linewidth=1,position=position_dodge(0)) +
#   geom_line(linewidth=0.8) + 
#   geom_vline(xintercept = 5,linetype="dashed") +
#   geom_vline(xintercept = 9,linetype="dashed") +
#   scale_color_manual(values=viridis(8, option = "H",direction=-1)) +
#   facet_nested_wrap(code~.,ncol=3,strip.position="top",scales="free") + #,scales="free_y"
#   ylab("Monthly precipitation (mm)") +
#   xlab("Months")
# p
#save
# out.file <- paste0(output_folder,"/plot6_clusterDistances.jpg")
# jpeg(out.file,width = 17, height = 10, units = "cm", res = 330)
# p
# dev.off()









