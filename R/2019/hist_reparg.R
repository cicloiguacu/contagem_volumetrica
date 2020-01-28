# 
# bici historico
#
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(dplyr)
library(data.table)
sete <- openxlsx::read.xlsx("data/2018/rep_argentina.xlsx", sheet = 3)
sete <- setDT(sete)

sete[Ano %in% 2016, Ano:= 2017]
sete[,local := "Av. República Argentina x Av. Getúlio Vargas"]
# sete[Local %in% "Via Lenta",lab := Total/2]
# a <- sete2[Local %in% "Via Lenta",][,lab := Total/2][,Total]
# sete2[Local %in% "Canaleta",lab:= Total/2 + a]
#sete2[,lab := cumsum(Total),by = .(Via.Lenta,Ano)]
#sete2[Local %in% "Canaleta",][,lab:= 2*lab + (100 - lab)*0.5]
#sete1[Local %in% "Via Lenta",][,lab := Total/2]
#sete1[Local %in% "Canaleta", ][,lab := ]
#break()
ggplot(sete,
       aes(x = Ano,y = Total,fill = Veiculo)) +
  geom_bar(stat = "identity") +
  xlab(NULL)+
  #ylim(c(0,1.05 * max(sete1$Total)))+
  facet_grid(rows = vars(Veiculo),cols = vars(local),scales = "free")+
  geom_text(data = sete,
            aes(x = Ano, y = Total,label= Total),
            vjust = 1.5,colour = "grey8",size = 3) +
  scale_x_continuous(breaks = c(2017:2019),
                     labels = c(2016,2018:2019)) +
  theme(axis.text.x = element_text(size = 9, angle = 0),
        legend.position = "none")

ggsave(filename =paste0("graphics/historico/total_republica_argentina.jpg"), scale = 1.2,
       width = 7.5,height = 10,units = "cm",dpi = "print")
