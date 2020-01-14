# intro
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(gridExtra)
sname <- openxlsx::getSheetNames("data/2019/contagens2019.xlsx")
# centro
dt <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[1],startRow=8)
dt[is.na(dt)] <- 0
dt$CAMINHAO_TOTAL <- dt$`CAMINHÃO.(2.EIXOS)`+dt$`CAMINHÃO.(+.2.EIXOS)`
tempo <- stringr::str_split_fixed(dt$`Período:.manhã`,"-",2)[,1]
nomes <- c("Bicicleta","Automóvel","Motocicleta","Ônibus","Caminhão","Outro")
tempo <- stringr::str_split_fixed(dt$`Período:.manhã`,"-",2)[,1] %>% 
  as.ITime() %>% as.POSIXct() %>% format("%H:%M")
dtcen <- data.frame("time" = rep(1:64,6),
                    "veh" = rep(nomes,each=nrow(dt)),
                    "total" = c(dt$BICI,dt$AUTO,dt$MOTO,dt$ÔNIBUS,dt$CAMINHAO_TOTAL,dt$OUTRO),
                    "way" = "Centro");rm(dt)
dtcenpie <- dtcen
dtcenpie <- as.data.table(dtcenpie)[,total := sum(total), by = veh][,.SD[1],by=veh][order(veh),]
dtcenpie <- dtcenpie[,total := 100 * total/sum(total)][,total := round(total,1)]
dtcenpie[,ymax := cumsum(dtcenpie$total)][,ymin := c(0,head(ymax,-1))]
dtcenpie[,pos := (ymax+ymin)/2]
# bairro
dt <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[2],startRow=8)
dt[is.na(dt)] <- 0
dt$CAMINHAO_TOTAL <- dt$`CAMINHÃO.(2.EIXOS)`+dt$`CAMINHÃO.(+.2.EIXOS)`
nomes <- c("Bicicleta","Automóvel","Motocicleta","Ônibus","Caminhão","Outro")
dtbai <- data.frame("time" = rep(1:64,6),
                    "veh" = rep(nomes,each=nrow(dt)),
                    "total" = c(dt$BICI,dt$AUTO,dt$MOTO,dt$ÔNIBUS,dt$CAMINHAO_TOTAL,dt$OUTRO),
                    "way" = "Bairro");rm(dt)
dtbaipie <- dtbai
dtbaipie <- as.data.table(dtbaipie)[,total := sum(total),by=veh][,.SD[1],by = veh][order(veh),]
dtbaipie <- dtbaipie[,total := 100 * total/sum(total)][,total := round(total,1)]
dtbaipie[,ymax := cumsum(dtbaipie$total)][,ymin := c(0,head(ymax,-1))]
dtbaipie[,pos := (ymax+ymin)/2]
# merge
dthour <- rbind(dtcen,dtbai)
dtpie <- rbind(dtcenpie,dtbaipie)
# ggplot pie
pie <- ggplot(dtpie,aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3,fill = veh)) +
  geom_rect() +
  coord_polar(theta = "y")+
  facet_grid(rows = vars(way)) +
  geom_label(x = rep(c(rep(3.4,3),3.5,3.5,3.8),2),
             aes(y = pos, label = paste0(total,"%")), size = 3,show.legend = FALSE) +
  xlim(c(2,4)) + ylab(NULL) +
  theme_grey() +
  theme(axis.text = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.x.top = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.text.y = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(fill = "Tipo de veículo")
# ggplot hour
aux <- as.data.table(dthour)
aux <- aux[,unique := paste0(time,"_",way)][,sum_unique := sum(total),by = unique][,.SD[1],by=unique]
hour <- ggplot(dthour,aes(x = time,y = total,fill = veh)) +
  geom_bar(stat = "identity") +
  # geom_text(data = aux,aes(y = sum_unique,label=sum_unique),hjust=-0.25,angle=90,
  #           colour = "grey58",size=3,fontface = "bold") +
  scale_x_continuous(breaks = seq(1,64,3),
                     labels = tempo[seq(1,64,3)]) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 8),
        legend.position = "none")+
  labs(fill = "Local de \n circulação")+
  xlab(NULL)+ylab("Número de bicicletas")+
  facet_grid(rows = vars(way),scales = "free")
hour
# multiplot
pf <- grid.arrange(hour,pie,ncol=2)
ggsave(filename =paste0("graphics/total-flow_6to22/",sname[1],".jpg"),plot = pf,
       width = 35,height = 15,units = "cm",dpi = "print")


