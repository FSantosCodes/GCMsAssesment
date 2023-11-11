library(qmap)
library(MBC)
library(terra)
library(Metrics)
library(reshape)
library(scales)
library(ggnewscale)
library(RColorBrewer)
library(ggplot2)
options(scipen=999)

#read data
obs <- vect("F:/DATA_AUX1/sensitivityModels/1_data/inamhi/Isoyeta 1981-2010/ISOYETAS_1981-2010_GWS.shp")
mod <- rast(list.files("F:/DATA_AUX1/sensitivityModels/1_data/raw/historical",full.names=T,pattern="_prec"))
regional.shp <- vect("F:/DATA_AUX1/sensitivityModels/1_data/shp/regionalArea.shp")
#which method? NDM, EQM, ERM, QML, QDM
method_correction <- "EQM"
#output files
output_plot <- "F:/DATA_AUX1/sensitivityModels/3_plots/plot0_biasCorrection.jpg"
output_biasCorrected <- "F:/DATA_AUX1/sensitivityModels/1_data/raw/fixed_historical/precAnnual_corrected.tif"
output_raw <- "F:/DATA_AUX1/sensitivityModels/1_data/raw/fixed_historical/precAnnual_raw.tif"

#### START PROCESSING ####

#start cropping and sum
mod <- crop(mod,regional.shp)
mod <- sum(mod)
mod <- mask(mod,regional.shp)
writeRaster(mod,output_raw,overwrite=T)
mask.na <- !is.na(values(mod))
#extract
obs$mod <- extract(mod,obs)[,2]
#get values
obs.val <- obs$ACUMULADO
mod.val <- obs$mod
#remove nas
obs.val <- obs.val[!is.na(mod.val)]
mod.val <- mod.val[!is.na(mod.val)]
#qm1
qm1.fit <- fitQmap(obs.val, mod.val, method = "PTF", transfun = "linear", wet.day =FALSE, cost = "RSS")
qm1.mod <- doQmapPTF(mod.val, qm1.fit)
#qm2
qm2.fit <- fitQmapQUANT(obs.val, mod.val, qstep = 0.01, wet.day = FALSE)
qm2.mod <- doQmapQUANT(mod.val, qm2.fit)
#qm3
qm3.fit <- fitQmap(obs.val, mod.val, qstep = 0.01, method = "RQUANT", wet.day = FALSE)
qm3.mod <- doQmap(mod.val, qm3.fit, type = "linear")
#qm4
qm4.fit <- fitQmap(obs.val, mod.val, qstep = 0.01, method = "DIST", dist = "norm",
                   wet.day = FALSE, optim.method = "CG")
qm4.mod <- doQmap(mod.val, qm4.fit)
#apply MBC
qm5.mod <- QDM(o.c = obs.val,
               m.c = mod.val,
               m.p = mod.val,
               ratio= T,
               jitter.factor = 0.01)$mhat.p
#plot
plot.df <- data.frame(NDM=qm1.mod,
                      ERM=qm3.mod,
                      QML=qm4.mod,
                      QDM=qm5.mod,
                      EQM=qm2.mod,
                      obs=obs.val,
                      raw=mod.val)
plot.df <- melt(plot.df)
plot.df$variable
p <- ggplot(plot.df,aes(x=value,group=variable,color=variable,linetype=variable)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45,size=8,hjust = 1),
        axis.text.y = element_text(size=8),
        panel.grid.minor.y = element_blank()) +
  geom_density(linewidth=1) +
  scale_color_manual(values=c(hue_pal()(5),"black","gray60")) +
  scale_linetype_manual(values=c(rep("solid",5),"dashed","dashed")) +
  xlab("Annual precipitation (mm)") +
  ylab("Density") +
  guides(color=guide_legend(title="Bias-correction \n methods",nrow=1),
         linetype=guide_legend(title="Bias-correction \n methods",nrow=1))

p
#get stats
stats.df <- data.frame(NDM=qm1.mod,
                       EQM=qm2.mod,
                       ERM=qm3.mod,
                       QML=qm4.mod,
                       QDM=qm5.mod,
                       obs=obs.val,
                       raw=mod.val)
stats.df <- data.frame(mean=unlist(lapply(stats.df,mean)),
                          sd=unlist(lapply(stats.df,sd)))
stats.df$corr <- rownames(stats.df)
rownames(stats.df) <- NULL
#compute metrics
val.df <- data.frame(NDM=qm1.mod,
                     EQM=qm2.mod,
                     ERM=qm3.mod,
                     QML=qm4.mod,
                     QDM=qm5.mod,
                     obs=obs.val,
                     raw=mod.val)
escore.ls <- list()
for(i in 1:ncol(val.df)){
  escore.ls[[i]] <- escore(as.matrix(obs.val),as.matrix(val.df[,i]))
}
val.df <- data.frame(corr=colnames(val.df),escore=unlist(escore.ls))
val.df <- val.df[order(val.df$escore),]
val.df
#apply best method
mask.na <- !is.na(values(mod))
if(method_correction=="QDM"){
  values(mod)[mask.na] <- QDM(o.c = obs.val,
                              m.c = values(mod)[mask.na],
                              m.p = values(mod)[mask.na],
                              ratio= T)$mhat.c
}else if(method_correction=="NDM"){
  values(mod)[mask.na] <- doQmap(values(mod)[mask.na], qm1.fit)
}else if(method_correction=="EQM"){
  values(mod)[mask.na] <- doQmapQUANT(values(mod)[mask.na], qm2.fit)
}else if(method_correction=="ERM"){
  values(mod)[mask.na] <- doQmap(values(mod)[mask.na], qm3.fit, type = "linear") 
}else if(method_correction=="QML"){
  values(mod)[mask.na] <- doQmap(values(mod)[mask.na], qm4.fit)
}else{
  stop("method unkown")
}
#save model
writeRaster(mod,output_biasCorrected,overwrite=T)
#save plot
out.file <- output_plot
jpeg(out.file,width = 17, height = 10, units = "cm", res = 330)
p
dev.off()

#### VALIDATE

#annual prec
sum.cor <- rast("F:/DATA_AUX1/sensitivityModels/1_data/raw/fixed_historical/precAnnual_corrected.tif")
sum.raw <- rast("F:/DATA_AUX1/sensitivityModels/1_data/raw/fixed_historical/precAnnual_raw.tif")
studyAreas_shp <- "F:/DATA_AUX1/sensitivityModels/1_data/shp/biogeographic_sectors_GWS.shp"
#check
inamhi.est <- vect("F:/DATA_AUX1/sensitivityModels/1_data/inamhi/Isoyeta 1981-2010/ISOYETAS_1981-2010_GWS.shp")
inamhi.est$raw <- extract(sum.raw,inamhi.est)[,2]
inamhi.est$cor <- extract(sum.cor,inamhi.est)[,2]
#global correlation & RMSE
cor.global <- c(round(cor(inamhi.est$ACUMULADO,inamhi.est$raw,method="spearman"),1),
                round(cor(inamhi.est$ACUMULADO,inamhi.est$cor,method="spearman"),1))
rmse.global <- c(round(rmse(inamhi.est$ACUMULADO,inamhi.est$raw),1),round(rmse(inamhi.est$ACUMULADO,inamhi.est$cor),1))
global.df <- data.frame(method=c("raw",method_correction),
                        cor=cor.global,
                        rmse=rmse.global)
#check by study areas
study.areas <- vect(studyAreas_shp)
study.areas <- intersect(study.areas,inamhi.est)
study.areas$raw <- extract(sum.raw,study.areas)[,2]
study.areas$cor <- extract(sum.cor,study.areas)[,2]
study.areas <- as.data.frame(study.areas)
study.areas <- split(study.areas,study.areas$CODE)
#compute metrics
areas.metrics <- list()
for(i in 1:length(study.areas)){
  area.i <- study.areas[[i]]
  metrics.corr <- c(round(cor(area.i$ACUMULADO,area.i$raw,method="spearman"),1),round(cor(area.i$ACUMULADO,area.i$cor,method="spearman"),1))
  metrics.rmse <- c(round(rmse(area.i$ACUMULADO,area.i$raw),1),round(rmse(area.i$ACUMULADO,area.i$cor),1))
  metrics.df <- data.frame(method=c("raw",method_correction),
    corr=metrics.corr,rmse=metrics.rmse, code = names(study.areas)[i])
  areas.metrics[[i]] <- metrics.df
}
areas.metrics <- do.call("rbind.data.frame",areas.metrics)
areas.metrics <- split(areas.metrics,areas.metrics$method)
val.df
areas.metrics
global.df
