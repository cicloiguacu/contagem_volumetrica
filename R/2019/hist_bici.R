# 
# bici historico
#
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(dplyr)
library(data.table)

sete <- openxlsx::read.xlsx("data/2018/rep_argentina.xlsx", sheet = 2)
sete
#sete <- setDT(sete)
#colnames(sete) <- c("Ano","Bento Vianna","Lamenha Lins")
sete <- data.table::data.table("Ano"= rep(sete$X1,4),
                               "veh" = rep(c("Bicicleta","Automóvel"),each = 16),
                                  "Total" = c(sete$bem_bici,sete$lam_bici,
                                              sete$bem_car,sete$lam_car),
                               "Local" = rep(rep(c("Bento Vianna","Lamenha Lins"),each=8),2))
sete[sete$Ano %in% 2008,Ano := 2012]
#sete[,total := sum()]

p <- ggplot(sete,
       aes(x = Ano,y = Total,fill = veh)) +
  geom_bar(stat = "identity") +
  xlab(NULL)+
  #ylim(c(0,1.05 * max(sete$Total)))+
  facet_grid(cols = vars(Local),rows = vars(veh),scales = "free") +
  geom_text(data = sete,
            aes(y = Total,label= Total),
            vjust = 1.5,colour = "grey15",size = 2.5) +
   scale_x_continuous(breaks = c(2012:2019),
                      labels = c(2008,2013:2019)) +
  theme(axis.text.x = element_text(size = 8, angle = 45),
        legend.position = "none")

ggsave(filename =paste0("graphics/historico/sete_vehicles.jpg"),plot = p, scale = 1.2,
       width = 17.5,height = 12,units = "cm",dpi = "print")
