library(ggplot2)
library(terra)
library(reshape2)
library(ggh4x)
library(ggstats)
options(scipen = 999999)

#study areas shapefile
studyAreas_shp <- "F:/DATA_AUX1/sensitivityModels/1_data/shp/biogeographic_sectors_GWS.shp"
#regional shapefile
regional_shp <- "F:/DATA_AUX1/sensitivityModels/1_data/shp/TM_WORLD_BORDERS-0.3_V8_countries.shp"
#input folders
extremes_folder <- "F:/DATA_AUX1/sensitivityModels/1_data/processed/extremes"
#output folder
output_folder <- "F:/DATA_AUX1/sensitivityModels/3_plots"

#### PREPARE DATA ####

#get files
extreme.rast <- list.files(extremes_folder,full.names=T,pattern=".tif$")
#hist.rast <- rast(grep("historical_prec",extreme.rast,value=T))
extreme.rast <- extreme.rast[-grep("historical_prec",extreme.rast,value=F)]
#study areas
areas.shp <- vect(studyAreas_shp)
#extract regional
regional.shp <- vect(regional_shp)
regional.shp$areaOriginal <- expanse(regional.shp,unit="km")
regional.shp <- crop(regional.shp,ext(c(-93.237,-64.006,-18.756,13.227)))
regional.shp$areaRemain <- expanse(regional.shp,unit="km")
regional.shp$areaPer <- round((regional.shp$areaRemain*100)/regional.shp$areaOriginal,1)
#regional.shp <- regional.shp[regional.shp$areaPer>5,]
regional.shp <- regional.shp[regional.shp$areaRemain>50000,]
as.data.frame(regional.shp)
#extract
extreme.areas <- list()
extreme.regional <- list()
for(i in 1:length(extreme.rast)){
  extreme.i <- rast(extreme.rast[i])
  extreme.name.i <- unlist(strsplit(gsub(".tif|ssp","",basename(extreme.rast[i])),"_"))
  #study areas
  extreme.areas.df <- list()
  for(j in 1:nrow(areas.shp)){
    extreme.area.j <- crop(extreme.i,ext(areas.shp[j]))
    extreme.area.j <- mask(extreme.area.j,areas.shp[j])
    extreme.area.j <- values(extreme.area.j)
    extreme.area.j <- c(round(mean(extreme.area.j,na.rm=T),1),round(sd(extreme.area.j,na.rm=T),1))
    code.j <- as.data.frame(areas.shp[j])$CODE
    extreme.areas.df[[j]] <- data.frame(code=code.j,
                                        type=extreme.name.i[1],
                                        ssp=extreme.name.i[2],
                                        mn=extreme.area.j[1],
                                        sd=extreme.area.j[2])
  }
  extreme.areas.df <- do.call("rbind.data.frame",extreme.areas.df)
  #regional
  extreme.regional.df <- list()
  for(j in 1:nrow(regional.shp)){
    extreme.regional.j <- crop(extreme.i,ext(regional.shp[j]))
    
    extreme.regional.j <- mask(extreme.regional.j,regional.shp[j])
    
    extreme.regional.j <- values(extreme.regional.j)
    extreme.regional.j <- c(round(mean(extreme.regional.j,na.rm=T),1),round(sd(extreme.regional.j,na.rm=T),1))
    code.j <- as.data.frame(regional.shp[j])$NAME
    extreme.regional.df[[j]] <- data.frame(code=code.j,
                                        type=extreme.name.i[1],
                                        ssp=extreme.name.i[2],
                                        mn=extreme.regional.j[1],
                                        sd=extreme.regional.j[2])
  }
  extreme.regional.df <- do.call("rbind.data.frame",extreme.regional.df)
  #add
  extreme.areas[[i]] <- extreme.areas.df
  extreme.regional[[i]] <- extreme.regional.df
}
rm(extreme.areas.df,extreme.regional.df,extreme.regional.j,extreme.area.j,extreme.i)
#consolidate
extreme.areas <- do.call("rbind.data.frame",extreme.areas)
extreme.regional <- do.call("rbind.data.frame",extreme.regional)

#### PLOT ####

#regional
regional.plot <- extreme.regional
regional.plot$type <- factor(regional.plot$type,labels=c("DRIEST","WETTEST"))
regional.plot$ssp <- factor(regional.plot$ssp,labels=c("1-2.6","2-4.5","3-7.0","5-8.5"))
regional.plot$ext <- "REGIONAL CONTEXT"
q <- ggplot(regional.plot,aes(x=ssp,y=mn,color=code,fill=code,group=code,shape=code)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8), 
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  geom_errorbar(aes(x=ssp, ymin=mn-sd, ymax=mn+sd),position=position_dodge2(width = 0.9,reverse=F),linewidth=0.5) +
  geom_point(position=position_dodge2(width = 0.9,reverse=F),color="black",size=2) +
  scale_shape_manual(values=c(21,22,23,24,25,21,22,23,24)) +
  geom_hline(yintercept = 0,linetype="dashed",color="black") +
  scale_x_discrete(expand = c(0.125, 0.125)) + 
  xlab("SSP") +
  ylab("Annual precipitation anomalies (mm)") +
  facet_nested_wrap(ext~type) +
  guides(color = guide_legend(title="Countries",nrow = 2),
         shape = guide_legend(title="Countries",nrow = 2),
         fill = guide_legend(title="Countries",nrow = 2)) +
  geom_stripped_cols(color=NA)
#save
# out.file <- paste0(output_folder,"/plot6_regional.jpg")
# jpeg(out.file,width = 17, height = 10, units = "cm", res = 330)
# q
# dev.off()
#check values
driest.val <- regional.plot[regional.plot$type=="dry",]
driest.val[order(driest.val$mn),]

wettest.val <- regional.plot[regional.plot$type=="wet",]
wettest.val[order(wettest.val$mn),]


#study areas
areas.plot <- extreme.areas
areas.plot$type <- factor(areas.plot$type,labels=c("DRIEST","WETTEST"))
areas.plot$ssp <- factor(areas.plot$ssp,labels=c("1-2.6","2-4.5","3-7.0","5-8.5"))
areas.plot$ext <- "ECUADOR STUDY AREAS"
p <- ggplot(areas.plot,aes(x=ssp,y=mn,color=code,fill=code,group=code,shape=code)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8), 
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  geom_errorbar(aes(x=ssp, ymin=mn-sd, ymax=mn+sd),position=position_dodge2(width = 0.9,reverse=F),linewidth=0.5) +
  geom_point(position=position_dodge2(width = 0.9,reverse=F),color="black",size=2) +
  geom_hline(yintercept = 0,linetype="dashed",color="black") +
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_x_discrete(expand = c(0.125, 0.125)) + 
  xlab("SSP") +
  ylab("Annual precipitation anomalies (mm)") +
  facet_nested_wrap(ext~type) +
  guides(color = guide_legend(title="Study areas",nrow = 1),
         shape = guide_legend(title="Study areas",nrow = 1),
         fill = guide_legend(title="Study areas",nrow = 1)) +
  geom_stripped_cols(color=NA)
#save
 out.file <- paste0(output_folder,"/plot7_studyAreas.jpg")
 jpeg(out.file,width = 17, height = 10, units = "cm", res = 330)
 p
 dev.off()

#check values
driest.stu <- areas.plot[areas.plot$type=="dry",]
driest.stu[order(driest.stu$mn),]

wettest.stu <- areas.plot[areas.plot$type=="wet",]
wettest.stu[order(wettest.stu$mn),]

#sum zero values
zero.dry <- extreme.rast[grep("dry",extreme.rast)]
dry.per <- list()
for(i in 1:length(zero.dry)){
  dry.i <- values(rast(zero.dry[i]))
  dry.i <- dry.i[!is.na(dry.i)]
  dry.cut <- dry.i[dry.i >= -80 & dry.i <= 80]
  dry.cut <- (length(dry.cut)*100)/length(dry.i)
  dry.per[[i]] <- dry.cut
}
dry.per

zero.wet <- extreme.rast[grep("wet",extreme.rast)]
wet.per <- list()
for(i in 1:length(zero.wet)){
  wet.i <- values(rast(zero.wet[i]))
  wet.i <- wet.i[!is.na(wet.i)]
  wet.cut <- wet.i[wet.i >= -80 & wet.i <= 80]
  wet.cut <- (length(wet.cut)*100)/length(wet.i)
  wet.per[[i]] <- wet.cut
}
wet.per


