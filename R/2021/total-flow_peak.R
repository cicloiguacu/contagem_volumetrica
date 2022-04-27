rm(list=ls())
library(readODS)
library(ggplot2)
library(patchwork)
library(magrittr)
library(data.table)
library(facetscales)
library(readr)

# read 2021----
dt_total_raw <- readr::read_rds("data/2021/contagem_2021.rds")
dt_total_raw
dt_total_raw
dt_total <- data.table::copy(dt_total_raw) %>% 
  .[veiculo != "Outro",] %>% 
  .[,sum(total),by =.(periodo,horario,veiculo)] %>% 
  .[,total_label := sum(V1),by = .(periodo,horario)] %>% 
  data.table::setnames(.,"V1","total")



hour_plot <- ggplot(dt_total)+
  geom_bar(aes(x=horario,y=total,fill=veiculo),stat="identity")+ 
  geom_text(data = dt_total[,.SD[1],by = .(periodo,horario)]
            ,aes(x=horario,y=total_label,label=total_label),vjust= 1.5,
            colour = "grey18",size=3,fontface = "bold")+
  #ylim(0,max(aux$sum_unique)*1.1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8),
  #       legend.position = "none")+
  labs(fill="Tipo de \n veículo")+
  xlab(NULL)+ylab("Número de veículos")+
  facet_wrap(~periodo,ncol = 1,scales = "free")

hour_plot  
# --
# PIE
# --
dtcenpie <- data.table::copy(dt_total_raw) %>% 
  .[veiculo != "Outro",] %>% 
  .[,total := sum(total),by = .(veiculo,periodo)] %>% 
  .[,.SD[1],by = .(veiculo,periodo)] %>% 
  .[,total_rel := 100 * total/sum(total),by = .(periodo)] %>% 
  .[,horario := NULL] %>% 
  .[,local := NULL] %>% 
  .[,gender := NULL] %>% 
  .[,variable := NULL] %>% 
  .[,way := NULL] %>% 
  .[,total_rel := round(total_rel,1)] %>% 
  .[,total := NULL] %>% 
  .[,ymax := cumsum(total_rel),by = .(periodo)] %>% 
  .[,ymin := c(0,head(ymax,-1)),by = .(periodo)] %>% 
  .[,pos := (ymax+ymin)/2,by=periodo]
dtcenpie
dtcenpie
# remove zero
#dtpie <- dtpie[total>0,]
#
# ggplot pie
pos_label <- rep(c(3.5,3.3,3.7,3.1,3.7,3.3,3.3,3.1),2)
ggplot(dtcenpie)+
  geom_rect(aes(ymax= ymax
                , ymin=ymin
                , xmax=4
                , xmin=3
                ,fill=veiculo)) +
  coord_polar(theta="y")+
  facet_grid(rows = vars(periodo))+
  geom_label(x= pos_label,aes(y = pos, label= paste0(total_rel,"%"))
             , size=3
             ,show.legend=FALSE)+
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
pie_plot
# multiplot
pf <- grid.arrange(hour,pie,ncol=2)
ggsave(filename =paste0("graphics/2021/hour_plot.jpg"),plot = hour_plot,
       width = 20,height = 15,units = "cm",dpi = "print")
print(sname[i])

}