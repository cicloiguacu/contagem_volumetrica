# intro
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(data.table)
#sname <- openxlsx::getSheetNames("data/2019/contagens2019.xlsx")

# centro
dtcen <- readODS::read_ods("data/2019/mal_floriano.ods",sheet = "Centro", col_names = TRUE)
dtcen[is.na(dtcen)] <- 0
dtcen$periodo <- dtcen[,1]
dtcen <- setDT(dtcen)
#dtcen <- setDT(dtcen)[,1:13]
#dtcen <- rbind(dtcen,data.table(c("total",colSums(dtcen[,2:11])))
dtcen[,vc_total := vc_masc + vc_fem + vc_nid]
dtcen[,cm_total := cm_masc + cm_fem + cm_nid]
dtcen[,can_total := can_masc + can_fem + can_nid]
dtcen[,cal_total := cal_masc + cal_fem + cal_nid]
dtcen[,BICI := vc_total + cm_total + can_total]
dtcen <- dtcen[,c("periodo","vc_total","cm_total","can_total","cal_total","BICI")]

tempo <- stringr::str_split_fixed(dtcen$periodo,"-",2)[,1] %>% 
  as.ITime() %>% as.POSIXct() %>% format("%H:%M")
# nomes <- c("Via Tr�fego \n Geral","Contra-m�o","Canaleta","Cal�ada")
nomes <- c("Canaleta","Cal�ada","Contra-m�o","Via Tr�fego \n Geral")
dtcen1 <- data.table("time" = rep(tempo,4),
                     "periodo" = rep(rep(c("Manh�","Tarde"),each=12),4),
                     "local" = rep(nomes,each=nrow(dtcen)),
                     #"total" = c(dtcen$vc_total,dtcen$cm_total,dtcen$can_total,dtcen$cal_total),
                     "total" =  c(dtcen$can_total,dtcen$cal_total,dtcen$cm_total,dtcen$vc_total),
                     "way" = "Centro")

#  bairro
dtbai <- readODS::read_ods("data/2019/mal_floriano.ods",sheet = "Bairro", col_names = TRUE)
dtbai[is.na(dtbai)] <- 0
dtbai$periodo <- dtbai[,1]
#dtbai <- setDT(dtbai)[,1:13]
dtbai <- setDT(dtbai)
#dtbai <- rbind(dtbai,data.table(c("total",colSums(dtbai[,2:11])))
#dtbai[,vc_total := vc_masc + vc_fem + vc_nid]
dtbai[,cal_total := cal_masc + cal_fem + cal_nid]
dtbai[,cm_total := cm_masc + cm_fem + cm_nid]
dtbai[,can_total := can_masc + can_fem + can_nid]
dtbai[,vc_total := vc_masc + vc_fem + vc_nid]
dtbai[,BICI := cal_total + cm_total + can_total + vc_total]
dtbai <- dtbai[,c("periodo","vc_total","cal_total","cm_total","can_total","BICI")]

tempo <- stringr::str_split_fixed(dtbai$periodo,"-",2)[,1] %>% 
  as.ITime() %>% as.POSIXct() %>% format("%H:%M")
# nomes <- c("Via Tr�fego \n Geral","Cal�ada","Contra-m�o","Canaleta")
nomes <- c("Canaleta","Cal�ada","Contra-m�o","Via Tr�fego \n Geral")
dtbai1 <- data.table("time" = rep(tempo,4),
                     "periodo" = rep(rep(c("Manh�","Tarde"),each=12),4),
                     "local" = rep(nomes,each=nrow(dtbai)),
                     "total" = c(dtbai$can_total,dtbai$cal_total,dtbai$cm_total,dtbai$vc_total),
                     "way" = "Bairro")
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
  labs(fill="Local de \n circula��o")+
  xlab(NULL)+ylab("N�mero de bicicletas")+
  facet_grid(rows = vars(way),cols = vars(periodo),scales = "free")
hour
break()
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
#
# merge
dtpie <- rbind(dtcenpie,dtbaipie)
# remove zero
#dtpie <- dtpie[total>0,]
#
# ggplot pie
pos_label <- c(3.1,3.7,3.5,3.5) %>% rep(2)
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
pie
# multiplot
pf <- grid.arrange(hour,pie,ncol=2)
ggsave(filename =paste0("graphics/mal_floriano/28.11 mal + sil (centro).jpg"),plot = pf,
       width = 35,height = 12.5,units = "cm",dpi = "print")
# print(sname[9])


