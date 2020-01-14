# intro
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(data.table)
library(dplyr)
sname <- openxlsx::getSheetNames("data/2019/contagens2019.xlsx")
for(i in c(3,5,11,13,15)){ # 3,5,9,11,13,15
  
  # centro
  dtcen <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[i],startRow=8)
  dtcen[is.na(dtcen)] <- 0
  dtcen <- setDT(dtcen)[,1:11]
  #dtcen <- rbind(dtcen,data.table(c("total",colSums(dtcen[,2:11])))
  
  #dtcen <- dtcen[,c("Período:.manhã","vc_total","cm_total","can_total","BICI")]
  
  tempo <- stringr::str_split_fixed(dtcen$`Período:.manhã`,"-",2)[,1] %>% 
    data.table::as.ITime() %>% as.POSIXct() %>% format("%H:%M")
  nomes <- c("Via Calma","Contra-mão","Canaleta")
  dtcen1 <- data.table::data.table("time" = rep(tempo,3*2),
                       "periodo" = rep(rep(c("Manhã","Tarde"),each=12),6),
                       "local" = rep(rep(nomes,each=24),2),
                       "gender" = rep(c("Masculino","Feminino"),each=24*3),
                       "total" = c(dtcen$vc_masc,dtcen$cm_masc,dtcen$can_masc,
                                   dtcen$vc_fem,dtcen$cm_fem,dtcen$can_fem),
                       "way" = "Centro e Bairro")
  # centro
  dtbai <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[i+1],startRow=8)
  dtbai[is.na(dtbai)] <- 0
  dtbai <- setDT(dtbai)[,1:11]
  
  dtbai1 <- c(dtbai$vc_masc,dtbai$cm_masc,dtbai$can_masc,
              dtbai$vc_fem,dtbai$cm_fem,dtbai$can_fem)
  # soma
  # --
  dtcen1$total <- dtcen1$total+dtbai1
  # ggplot hour
  dthour <- dtcen1
  aux <- dthour[,unique:=paste0(time,"_",periodo,"_",gender)][,sum_unique:= sum(total),by=unique][,.SD[1],by=unique]
  hour <- ggplot(dthour,aes(x=time,y = total,fill=local))+
    geom_bar(stat="identity")+
    geom_text(data=aux,aes(y=sum_unique,label=sum_unique),vjust=-0.5,
              colour = "grey58",size=3,fontface = "bold")+
    ylim(0,max(dthour$total)*1.25)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8),legend.position = "none")+
    labs(fill="Local de \n circulação")+
    xlab(NULL)+ylab("Número de bicicletas")+
    facet_grid(rows = vars(gender),cols = vars(periodo),scales = "free_x")
  # --
  # PIE
  # --
  dtcenpie <- dtcen1
  dtcenpie[,aux:= paste0(local,"_",gender)]
  dtcenpie <- as.data.table(dtcenpie)[,total := sum(total), by = aux][,.SD[1],by=aux][order(gender),]
  dtcenpie <- dtcenpie[,total := 100 * total/sum(total),by = gender][,total := round(total,1)]
  dtcenpie[,ymax := cumsum(total),by = gender][,ymin := c(0,head(ymax,-1)),by = gender]
  dtcenpie[,pos := (ymax+ymin)/2,by=gender]
  dtcenpie
  # merge
  dtpie <- dtcenpie
  # remove zero
  #dtpie <- dtpie[total>0,]
  #
  # ggplot pie
  pos_label <- c(3.5,3.3,3.7) %>% 
    rep(2)
  pie <- ggplot(dtpie,aes(ymax= ymax, ymin=ymin, xmax=4, xmin=3,fill=local))+
    geom_rect() +
    coord_polar(theta="y")+
    facet_grid(rows = vars(gender))+
    geom_label(x= pos_label,
               aes(y=pos, label= paste0(total,"%")), size=3,show.legend=FALSE)+
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
  # multiplot
  pf <- grid.arrange(hour,pie,ncol=2)
  ggsave(filename =paste0("graphics/gender_peak/",sname[i],".jpg"),plot = pf,
         width = 35,height = 12.5,units = "cm",dpi = "print")
  print(sname[i])
  
}
