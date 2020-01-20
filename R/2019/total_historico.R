# 
# bici historico
#
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(dplyr)
library(data.table)
sete <- openxlsx::read.xlsx("data/2018/rep_argentina.xlsx", sheet = 2)

sete <- setDT(sete)[-which(Local %in% "Auto"),][-which(Local %in% "Total Bici"),]
sete[,Total := round(100*Total/sum(Total)) , by=.(Ano,Via.Lenta)]
sete[Ano %in% 2008,Ano := 2012]

sete2 <- sete
sete2[Local %in% "Via Lenta",lab := Total/2]
a <- sete2[Local %in% "Via Lenta",][,lab := Total/2][,Total]
sete2[Local %in% "Canaleta",lab:= Total/2 + a]
#sete2[,lab := cumsum(Total),by = .(Via.Lenta,Ano)]
#sete2[Local %in% "Canaleta",][,lab:= 2*lab + (100 - lab)*0.5]
#sete1[Local %in% "Via Lenta",][,lab := Total/2]
#sete1[Local %in% "Canaleta", ][,lab := ]
#break()
ggplot(sete,
            aes(x = Ano,y = Total,fill = Local)) +
  geom_bar(stat = "identity") +
  xlab(NULL)+
  #ylim(c(0,1.05 * max(sete1$Total)))+
  facet_grid(rows = 'Via.Lenta',scales = "free_x")+
  geom_text(data = sete2,
            aes(x = Ano, y = lab,label= paste0(round(Total,0),"%")),
            vjust = -0.5,colour = "black",size = 3) +
  scale_x_continuous(breaks = c(2012,2013:2019),
                     labels = c(2008,2013:2019)) +
  theme(axis.text.x = element_text(size = 9, angle = 0))

ggsave(filename =paste0("graphics/historico/canaleta_sete.jpg"), scale = 1.2,
       width = 15,height = 10,units = "cm",dpi = "print")
