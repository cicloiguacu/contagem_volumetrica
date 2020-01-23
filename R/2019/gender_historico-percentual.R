# 
# bici historico
#
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(dplyr)
library(data.table)
sete <- openxlsx::read.xlsx("data/2018/rep_argentina.xlsx", sheet = "gender")

sete1 <- data.table::data.table("Ano" = rep(sete$X1,2),
                                "Total" = c(sete$bem_fem,sete$lam_fem) %>% as.numeric(),
                                "Local" = rep(c("Bento Viana","Lamenha Lins"),each=6))
#sete[,lab := cumsum(Total),by = .(Via.Lenta,Ano)]
#sete[Local %in% "Canaleta",][,lab:= 2*lab + (100 - lab)*0.5]
#sete1[Local %in% "Via Lenta",][,lab := Total/2]
#sete1[Local %in% "Canaleta", ][,lab := ]
#break()
ggplot(sete1,
       aes(x = Ano,y = Total,fill = Local)) +
  geom_bar(stat = "identity") +
  xlab(NULL)+
  ylim(c(0,1.05 * max(sete1$Total)))+
  facet_grid(rows = 'Local')+
   geom_text(data = sete1,
             aes(x = Ano, y = Total,label= paste0(round(Total,1),"%")),
             vjust = -0.5,colour = "black",size = 3) +
  scale_x_continuous(breaks = c(2014:2019),
                     labels = c(2014:2019)) +
  theme(axis.text.x = element_text(size = 9, angle = 0),
        legend.position = "none")

ggsave(filename =paste0("graphics/historico/gender_sete.jpg"), scale = 1.2,
       width = 12,height = 10,units = "cm",dpi = "print")
