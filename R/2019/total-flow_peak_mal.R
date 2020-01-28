# intro
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(readODS)
library(dplyr)
library(gridExtra)
library(data.table)
#sname <- openxlsx::getSheetNames("data/2019/contagens2019.xlsx")

# centro
dt <- readODS::read_ods("data/2019/mal_floriano.ods",sheet = "Centro", col_names = TRUE)
dt[is.na(dt)] <- 0
dt$periodo <- dt[,1]
dt$CAMINHAO_TOTAL <- dt$`CAMINHÃO (2 EIXOS)`+dt$`CAMINHÃO (2 EIXOS)`
tempo <- stringr::str_split_fixed(dt$periodo,"-",2)[,1] %>% 
  as.ITime() %>% as.POSIXct() %>% format("%H:%M")
nomes <- c("Bicicleta","Automóvel","Motocicleta","Ônibus","Caminhão","Outro")
dtcen <- data.table("time" = rep(tempo,6),
                    "periodo" = rep(rep(c("Manhã","Tarde"),each=12),6),
                    "veh" = rep(nomes,each=nrow(dt)),
                    "total" = c(dt$BICI,dt$AUTOMÓVEL,dt$MOTO,dt$ÔNIBUS,dt$CAMINHAO_TOTAL,dt$OUTRO),
                    "way" = "Centro")

# bairro
dt <- readODS::read_ods("data/2019/mal_floriano.ods",sheet = "Bairro", col_names = TRUE)
dt[is.na(dt)] <- 0
# dt$CAMINHAO_TOTAL <- dt$`CAMINHÃO.(2.EIXOS)`+dt$`CAMINHÃO.(+.2.EIXOS)`
 nomes <- c("Bicicleta")
dtbai <- data.table("time" = rep(tempo,1),
                    "periodo" = rep(rep(c("Manhã","Tarde"),each=12),1),
                    "veh" = rep(nomes,each=nrow(dt)),
                    "total" = c(dt$BICI),
                    "way" = "Bairro");rm(dt)
 dthour <- rbind(dtcen,dtbai)
#dthour <- dtcen
# ggplot hour
#dthour1 <- as.data.table(dtcen)[way %in% "Centro",]
#tempo1 <- tempo[1:12]
aux <- dthour[,unique := paste0(time,"_",periodo,"_",way)][,sum_unique := sum(total),by=unique][,.SD[1],by=unique]
hour <- ggplot(dthour,aes(x=time,y=total,fill=veh))+
  geom_bar(stat="identity")+ 
  geom_text(data=aux,aes(y=sum_unique,label=sum_unique),vjust=-0.5,
            colour = "grey58",size=3,fontface = "bold")+
  ylim(0,max(aux$sum_unique)*1.1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8),
        legend.position = "none")+
  labs(fill="Tipo de \n veículo")+
  xlab(NULL)+ylab("Número de veículos")+
  facet_grid(rows = vars(way),cols = vars(periodo),scales = "free_x")
hour
# --
# PIE
# --
dtcenpie <- dtcen
dtcenpie <- as.data.table(dtcenpie)[,total := sum(total), by = veh][,.SD[1],by=veh][order(veh),]
dtcenpie <- dtcenpie[,total := 100 * total/sum(total)][,total := round(total,1)]
dtcenpie[,ymax := cumsum(dtcenpie$total)][,ymin := c(0,head(ymax,-1))]
dtcenpie[,pos := (ymax+ymin)/2]
# merge
dtpie <- dtcenpie
# remove zero
#dtpie <- dtpie[total>0,]
#
# ggplot pie
pos_label <- c(3.5,3.3,3.7,3.1,3.7,3.3) %>% 
  rep(1)
pie <- ggplot(dtpie,aes(ymax= ymax, ymin=ymin, xmax=4, xmin=3,fill=veh))+
  geom_rect() +
  coord_polar(theta="y")+
  facet_grid(rows = vars(way))+
  geom_label(x= pos_label,
             aes(y=pos, label=paste0(total,"%")), size=3,show.legend=FALSE)+
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
  labs(fill="Tipo de veículo")
pie
# multiplot
pf <- grid.arrange(hour,pie,ncol=2)
ggsave(filename =paste0("graphics/mal_floriano/flow_28.11 mal + sil (centro).jpg"),plot = pf,
       width = 35,height = 12.5,units = "cm",dpi = "print")
ggsave(filename =paste0("graphics/mal_floriano/pie1_flow_28.11 mal + sil (centro).jpg"),plot = pie,
       width = 15.5,height = 15.5,scale = 0.65,units = "cm",dpi = "print")
#print(sname[9])

