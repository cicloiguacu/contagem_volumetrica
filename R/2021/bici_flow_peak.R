

# "23.11 arg + get (centro)" "23.11 arg + get (bairro)"

rm(list=ls())
library(readODS)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(data.table)

sname <- readODS::list_ods_sheets("data/2021/Contagem 2021.ods")
sname



# centro
dtcen <- readODS::read_ods(path = "data/2021/Contagem 2021.ods",sheet = "23.11 arg + get (centro)",skip = 6)
dtcen
dtcen[is.na(dtcen)] <- 0
dtcen <- data.table::setDT(dtcen)[,1:11]

dtcen[,vc_total := vc_masc + vc_fem + vc_nid] # via calma
dtcen[,cm_total := cm_masc + cm_fem + cm_nid] # contra mao
dtcen[,can_total := can_masc + can_fem + can_nid] # canaleta
dtcen[,BICI := vc_total + cm_total + can_total]
dtcen <- dtcen[,c("","vc_total","cm_total","can_total","BICI")]
data.table::setnames(dtcen,"","Período:.manhã")

tempo <- stringr::str_split_fixed(dtcen$`Período:.manhã`,"-",2)[,1] %>% 
  as.ITime() %>% as.POSIXct() %>% format("%H:%M")
#nomes <- c("Via Calma","Contra-mão","Canaleta")
nomes <- c("Via Tráfego \n Geral","Contra-mão","Canaleta")
dtcen1 <- data.table("time" = rep(tempo,3),
                     "periodo" = rep(rep(c("Manhã","Tarde"),each=12),3),
                     "local" = rep(nomes,each=nrow(dtcen)),
                     "total" = c(dtcen$vc_total,dtcen$cm_total,dtcen$can_total),
                     "way" = "Centro")

# bairro----
dtbai <- readODS::read_ods(path = "data/2021/Contagem 2021.ods",sheet = "23.11 arg + get (bairro)",skip = 7)
dtbai[is.na(dtbai)] <- 0
dtbai <- setDT(dtbai)[,1:11]

dtbai[,vc_total := vc_masc + vc_fem + vc_nid] # via calma
dtbai[,cm_total := cm_masc + cm_fem + cm_nid] # contra mao
dtbai[,can_total := can_masc + can_fem + can_nid] # canaleta
dtbai[,BICI := vc_total + cm_total + can_total] # total
names(dtbai)
data.table::setnames(dtbai,"","Período:.manhã")
dtbai <- dtbai[,c("Período:.manhã","vc_total","cm_total","can_total","BICI")]

dtbai1 <- data.table("time" = rep(tempo,3),
                     "periodo" = rep(rep(c("Manhã","Tarde"),each=12),3),
                     "local" = rep(nomes,each=nrow(dtbai)),
                     "total" = c(dtbai$vc_total,dtbai$cm_total,dtbai$can_total),
                     "way" = "Bairro")
# ggplot hour
dthour <- rbind(dtcen1,dtbai1)
aux <- dthour[,unique := paste0(time,"_",periodo,"_",way)][,sum_unique := sum(total),by=unique][,.SD[1],by=unique]
hour <- ggplot(dthour,aes(x=time,y=total,fill=local))+
  geom_bar(stat="identity")+ 
  geom_text(data=aux,aes(y=sum_unique,label=sum_unique),vjust=-0.5,
            colour = "grey58",size=3,fontface = "bold")+
  ylim(0,max(aux$sum_unique)*1.1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8),
        legend.position = "none")+
  labs(fill="Local de \n circulação")+
  xlab(NULL)+ylab("Número de bicicletas")+
  facet_grid(rows = vars(way),cols = vars(periodo),scales = "free_x")
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
  labs(fill="Local de \n circulação")
# multiplot
pf <- grid.arrange(hour,pie,ncol=2)
dir.create("graphics/2021")
ggsave(filename =paste0("graphics/2021/23.11_arg-get.jpg"),plot = pf,
       width = 35,height = 15,units = "cm",dpi = "print")



