# intro
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(gridExtra)
sname <- openxlsx::getSheetNames("data/2019/contagens2019.xlsx")

# centro
dtcen <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[1],startRow=8)
dtcen[is.na(dtcen)] <- 0
dtcen <- setDT(dtcen)[,1:11]
#dtcen <- rbind(dtcen,data.table(c("total",colSums(dtcen[,2:11])))
dtcen[,vc_total := vc_masc + vc_fem + vc_nid]
dtcen[,cm_total := cm_masc + cm_fem + cm_nid]
dtcen[,can_total := can_masc + can_fem + can_nid]
dtcen[,BICI := vc_total + cm_total + can_total]
dtcen <- dtcen[,c("Per�odo:.manh�","vc_total","cm_total","can_total","BICI")]

tempo <- stringr::str_split_fixed(dtcen$`Per�odo:.manh�`,"-",2)[,1] %>% 
  as.ITime() %>% as.POSIXct() %>% format("%H:%M")
nomes <- c("Via Calma","Contra-m�o","Canaleta")
dtcen1 <- data.table("time" = rep(1:64,3),
                     "local" = rep(nomes,each=nrow(dtcen)),
                     "total" = c(dtcen$vc_total,dtcen$cm_total,dtcen$can_total),
                     "way" = "Centro")

# centro
dtbai <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[2],startRow=8)
dtbai[is.na(dtbai)] <- 0
dtbai <- setDT(dtbai)[,1:11]
#dtbai <- rbind(dtbai,data.table(c("total",colSums(dtbai[,2:11])))
dtbai[,vc_total := vc_masc + vc_fem + vc_nid]
dtbai[,cm_total := cm_masc + cm_fem + cm_nid]
dtbai[,can_total := can_masc + can_fem + can_nid]
dtbai[,BICI := vc_total + cm_total + can_total]
dtbai <- dtbai[,c("Per�odo:.manh�","vc_total","cm_total","can_total","BICI")]

dtbai1 <- data.table("time" = rep(1:64,3),
                     "local" = rep(nomes,each=nrow(dtbai)),
                     "total" = c(dtbai$vc_total,dtbai$cm_total,dtbai$can_total),
                     "way" = "Bairro")
# ggplot hour

dthour <- rbind(dtcen1,dtbai1)
aux <- setDT(dthour)[,unique := paste0(time,"_",way)][,sum_unique := sum(total),by = unique][,.SD[1],by=unique]
hour <- ggplot(dthour,aes(x = time,y = total,fill = local)) +
  geom_bar(stat = "identity",) +
  # geom_text(data = aux,aes(y = sum_unique,label=sum_unique),hjust=-0.25,angle=90,
  #           colour = "grey58",size=3,fontface = "bold") +
  scale_x_continuous(breaks = seq(1,64,3),
                   labels = tempo[seq(1,64,3)]) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 8),
        legend.position = "none")+
  labs(fill = "Local de \n circula��o")+
  xlab(NULL)+ylab("N�mero de bicicletas")+
  facet_grid(rows = vars(way),scales = "free")
# --
# PIE
# --
dtcenpie <- dtcen1
dtcenpie <- as.data.table(dtcenpie)[,total := sum(total), by = local][,.SD[1],by=local][order(local),]
dtcenpie <- dtcenpie[,total := 100 * total/sum(total)][,total := round(total,1)]
dtcenpie[,ymax := cumsum(dtcenpie$total)][,ymin := c(0,head(ymax,-1))]
dtcenpie[,pos := (ymax+ymin)/2]
#
dtbaipie <- dtbai1
dtbaipie <- as.data.table(dtbaipie)[,total := sum(total),by=local][,.SD[1],by = local][order(local),]
dtbaipie <- dtbaipie[,total := 100 * total/sum(total)][,total := round(total,1)]
dtbaipie[,ymax := cumsum(dtbaipie$total)][,ymin := c(0,head(ymax,-1))]
dtbaipie[,pos := (ymax+ymin)/2]
# merge
dtpie <- rbind(dtcenpie,dtbaipie)
# remove zero
#dtpie <- dtpie[total>0,]
#
# ggplot pie
pos_label <- c(3.5,3.5,3.5) %>% 
  rep(2)
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
  labs(fill="Local de \n circula��o")
# multiplot
pf <- grid.arrange(hour,pie,ncol=2)
ggsave(filename =paste0("graphics/bici_flow_6to22/",sname[3],".jpg"),plot = pf,
       width = 35,height = 15,units = "cm",dpi = "print")


