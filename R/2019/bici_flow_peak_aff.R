# intro
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(gridExtra)
sname <- openxlsx::getSheetNames("data/2019/contagens2019.xlsx")

# centro
dtcen <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[9],startRow=8)
dtcen[is.na(dtcen)] <- 0
dtcen <- setDT(dtcen)[,1:13]
#dtcen <- rbind(dtcen,data.table(c("total",colSums(dtcen[,2:11])))
dtcen[,vc_total := vc_masc + vc_fem + vc_nid]
dtcen[,ci_total := ci_masc + ci_fem + ci_nid]
dtcen[,cm_total := cm_masc + cm_fem + cm_nid]
dtcen[,can_total := can_masc + can_fem + can_nid]
dtcen[,BICI := vc_total + cm_total + can_total]
dtcen <- dtcen[,c("Período:.manhã","vc_total","cm_total","can_total","ci_total","BICI")]

tempo <- stringr::str_split_fixed(dtcen$`Período:.manhã`,"-",2)[,1] %>% 
  as.ITime() %>% as.POSIXct() %>% format("%H:%M")
nomes <- c("Via Tráfego \n Geral","Contra-mão","Canaleta","Ciclovia")
dtcen1 <- data.table("time" = rep(tempo,4),
                     "periodo" = rep(rep(c("Manhã","Tarde"),each=12),4),
                     "local" = rep(nomes,each=nrow(dtcen)),
                     "total" = c(dtcen$vc_total,dtcen$cm_total,dtcen$can_total,dtcen$ci_total),
                     "way" = "Ambos sentidos")

# centro
dtbai <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[10],startRow=8)
dtbai[is.na(dtbai)] <- 0
dtbai <- setDT(dtbai)[,1:13]
dtbai
#dtbai <- rbind(dtbai,data.table(c("total",colSums(dtbai[,2:11])))
dtbai[,vc_total := vc_masc + vc_fem + vc_nid]
dtbai[,ci_total := ci_masc + ci_fem + ci_nid]
dtbai[,cm_total := cm_masc + cm_fem + cm_nid]
dtbai[,can_total := can_masc + can_fem + can_nid]
dtbai[,BICI := vc_total + cm_total + can_total]
dtbai <- dtbai[,c("Período:.manhã","vc_total","cm_total","can_total","ci_total","BICI")]

tempo <- stringr::str_split_fixed(dtbai$`Período:.manhã`,"-",2)[,1] %>% 
  as.ITime() %>% as.POSIXct() %>% format("%H:%M")
nomes <- c("Via Calma","Contra-mão","Canaleta","Ciclovia")
dtbai1 <- data.table("time" = rep(tempo,4),
                     "periodo" = rep(rep(c("Manhã","Tarde"),each=12),4),
                     "local" = rep(nomes,each=nrow(dtbai)),
                     "total" = c(dtbai$vc_total,dtbai$cm_total,dtbai$can_total,dtbai$ci_total),
                     "way" = "Ambos sentidos")
# ggplot hour
dthour <- rbind(dtcen1,dtbai1)
aux <- dthour[,unique := paste0(time,"_",periodo,"_",way)][,sum_unique := sum(total),by=unique][,.SD[1],by=unique]
hour <- ggplot(dthour,aes(x=time,y=total,fill=local))+
  geom_bar(stat="identity")+ 
  geom_text(data=aux,aes(y=sum_unique,label=sum_unique),vjust=-0.5,
            colour = "grey58",size=3,fontface = "bold")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8),
         legend.position = "none")+
  ylim(0,max(aux$sum_unique)*1.1)+
  labs(fill="Local de \n circulação")+
  xlab(NULL)+ylab("Número de bicicletas")+
  facet_grid(rows = vars(way),cols = vars(periodo),scales = "free")
# --
# PIE
# --
dtcenpie <- dtcen1
dtcenpie <- as.data.table(dtcenpie)[,total := sum(total), by = local][,.SD[1],by=local][order(local),]
dtcenpie <- dtcenpie[,total := 100 * total/sum(total)][,total := round(total,1)]
dtcenpie[,ymax := cumsum(dtcenpie$total)][,ymin := c(0,head(ymax,-1))]
dtcenpie[,pos := (ymax+ymin)/2]
#
# merge
dtpie <- dtcenpie
# remove zero
#dtpie <- dtpie[total>0,]
#
# ggplot pie
pos_label <- c(3.5,3.5,3.5,3.5)
pie <- ggplot(dtpie,aes(ymax= ymax, ymin=ymin, xmax=4, xmin=3,fill=local))+
  geom_rect() +
  coord_polar(theta="y")+
  facet_grid(rows = vars(way))+
  geom_label(x= pos_label,
             aes(y=pos, label= paste0(total,"%")), size=3,show.legend=FALSE)+
  xlim(c(2,4))+
  ylab(NULL)+
  theme_grey()+
  theme(axis.text=element_blank(),
        axis.text.x=element_blank(), 
        axis.text.x.top=element_blank(),
        axis.text.x.bottom=element_blank(),
        axis.text.y=element_blank(),
        axis.text.y.left=element_blank(),
        axis.text.y.right=element_blank(),
        axis.ticks.y=element_blank())+
  labs(fill="Local de \n circulação")
# multiplot
pf <- grid.arrange(hour,pie,ncol=2)
ggsave(filename =paste0("graphics/affonso_camargo/",sname[9],".jpg"),plot = pf,
       width = 35,height = 7.5,units = "cm",dpi = "print")
print(sname[9])


