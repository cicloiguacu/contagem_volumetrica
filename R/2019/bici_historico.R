# 
# bici historico
#
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(dplyr)
library(data.table)
sete <- openxlsx::read.xlsx("data/2018/rep_argentina.xlsx", sheet = 2)

sete <- setDT(sete)[-which(Local %in% "Canaleta"),][-which(Local %in% "Via Lenta"),]
sete <- sete[Ano %between% c(2014,2019),]
#sete[,total := sum()]

p <- ggplot(sete,
       aes(x = Ano,y = Total,fill = Local)) +
  geom_bar(stat = "identity") +
  xlab(NULL)+
  #ylim(c(0,1.05 * max(sete$Total)))+
  facet_grid(cols = vars(Via.Lenta),rows = vars(Local),scales = "free") +
  geom_text(data = sete,
            aes(y = Total,label= Total),
            vjust = 1.5,colour = "grey15",size = 2.5) +
  scale_x_continuous(breaks = c(2014:2019),
                     labels = c(2014:2019)) +
  theme(axis.text.x = element_text(size = 8, angle = 0),
        legend.position = "none")
p
ggsave(filename =paste0("graphics/historico/sete_vehicles.jpg"),plot = p, scale = 1.2,
       width = 17.5,height = 12,units = "cm",dpi = "print")
