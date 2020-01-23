# 
# bici historico
#
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)
sete <- openxlsx::read.xlsx("data/2018/rep_argentina.xlsx", sheet = "gender_total")
sete
sete1 <- data.table::data.table("Ano" = rep(sete$X1,4),
                                "Total" = c(sete$lam_masc,sete$lam_fem,
                                            sete$bem_masc,sete$bem_fem) %>% as.numeric(),
                                "Gender" = rep(c("Masculino","Feminino"),each=6) %>% rep(2),
                                "Local" = rep(c("Lamenha Lins","Bento Viana"),each=12),
                                "Absoluto" = "Absoluto")
# segundo data
sete2 <- as.data.frame(sete1)
sete2 <- as.data.table(sete2)[,Total:= 100 * Total/sum(Total) %>% round(1),by=.(Ano,Local)]
sete2[,Absoluto := "Relativo"]
#sete[,lab := cumsum(Total),by = .(Via.Lenta,Ano)]
#sete[Local %in% "Canaleta",][,lab:= 2*lab + (100 - lab)*0.5]
#sete1[Local %in% "Via Lenta",][,lab := Total/2]
#sete1[Local %in% "Canaleta", ][,lab := ]
#break()
p1 <- ggplot(sete1,
       aes(x = Ano,y = Total,fill = Local)) +
  geom_col(position = "dodge") +
  xlab(NULL)+
  #ylim(c(0,1.05 * max(sete1$Total)))+
  facet_grid(rows = vars(Gender),col = vars(Absoluto),scales = "free")+
  geom_text(data = sete1,
            aes(x= Ano, y= Total,label= Total),
            position=position_dodge(width=0.9), vjust=+1.75,
            colour = "black",size = 4) +
  scale_x_continuous(breaks = c(2014:2019),
                     labels = c(2014:2019)) +
  theme(axis.text.x = element_text(size = 9, angle = 0),legend.position = "none")

#p1
p2 <- ggplot(sete2,
             aes(x = Ano,y = Total,fill = Local)) +
  geom_col(position = "dodge") +
  xlab(NULL)+
  #ylim(c(0,1.05 * max(sete1$Total)))+
  facet_grid(rows = vars(Gender),col = vars(Absoluto),scales = "free")+
  geom_text(data = sete2,
            aes(x= Ano, y= Total,label= paste0(round(Total,1),"%")),
            position=position_dodge(width=0.9), vjust=+1.75,
            colour = "black",size = 3.5) +
  scale_x_continuous(breaks = c(2014:2019),
                     labels = c(2014:2019)) +
  theme(axis.text.x = element_text(size = 9, angle = 0))

#p2

pf <- grid.arrange(p1,p2,ncol=2)

ggsave(pf,filename =paste0("graphics/historico/gender_abs-rel_sete.jpg"), scale = 1.2,
       width = 35,height = 15,units = "cm",dpi = "print")
