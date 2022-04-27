rm(list=ls())
library(readODS)
library(ggplot2)
library(patchwork)
library(magrittr)
library(data.table)
library(facetscales)
library(readr)

# read 2021----
dt_total <- readr::read_rds("data/2021/contagem_2021.rds")

# ggplot hour
dthour <- data.table::copy(dtbai)[veiculo == "Bicicleta" & gender != "N/ID"]

dthour[,total_label := sum(total),by = .(horario,periodo,gender)]
dthour

hour_plot <- ggplot()+
  geom_bar(data = dthour,aes(x=horario,y = total,fill=local),stat="identity")+
  geom_text(data = dthour[,.SD[1],by = .(horario,periodo,gender)]
            ,aes(x=horario,y=total_label,label=total_label),vjust=1.5,
            colour = "grey18",size=3,fontface = "bold")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8),legend.position = "none")+
  labs(fill="Local de \n circulação"
       ,y = "Número de bicicletas em ambos sentidos"
       ,x = NULL)+
  facet_grid(rows = vars(gender),cols = vars(periodo),scales = "free")
# PIE
# --
dtcenpie <- data.table::copy(dthour) %>% 
  .[,total := sum(total),by = .(local,gender)] %>% 
  .[,.SD[1],by = .(local,gender)] %>% 
  .[,periodo := NULL] %>% 
  .[,horario := NULL] %>% 
  .[,total_label := NULL] %>% 
  .[,total_rel := 100 * total/sum(total),by = .(gender)] %>% 
  .[,total_rel := round(total_rel,1)] %>% 
  .[,total := NULL] %>% 
  .[,ymax := cumsum(total_rel),by = gender] %>% 
  .[,ymin := c(0,head(ymax,-1)),by = gender] %>% 
  .[,pos := (ymax+ymin)/2,by=gender]
dtcenpie
dtcenpie
# merge
dtpie <- dtcenpie

# ggplot pie
pos_label <- rep(c(3.5,3.3,3.7),2)
pie_plot <- ggplot(dtpie)+
  geom_rect(aes(ymax= ymax
                , ymin=ymin
                , xmax=4
                , xmin=3
                ,fill=local)) +
  coord_polar(theta="y")+
  facet_grid(rows = vars(gender))+
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
  labs(fill="Local de \n circulação")
pie_plot
# multiplot
pie_plot
pf <- hour_plot + pie_plot +  plot_layout(widths = c(2, 1))
pf


ggsave(filename =paste0("graphics/2021/rep_arg_2021_genero.jpg"),plot = pf,
       width = 30,height = 15,units = "cm",dpi = "print")
