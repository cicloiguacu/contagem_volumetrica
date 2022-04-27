# intro
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(data.table)
library(dplyr)
sname <- openxlsx::getSheetNames("data/2019/contagens2019.xlsx")


# centro
DT2 <- lapply(c(1,3,5,11,13,15),function(i){ # 7,9
  dtcen <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[i],startRow=8)
  dtcen1 <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[i+1],startRow=8)
  dtcen2 <- rbind(dtcen[,1:12],dtcen1[,1:12]) 
  dtcen2[is.na(dtcen2)] <- 0
  
  t_masc <- dtcen2[,which(names(dtcen2) %like% "masc")] %>% sum()
  t_fem <-  dtcen2[,which(names(dtcen2) %like% "fem")] %>% sum()
  t_nid <-  dtcen2[,which(names(dtcen2) %like% "nid")] %>% sum()
  t_bici <- dtcen2[,which(names(dtcen2) %like% "BICI")] %>% sum()
  
  DT2 <- data.table::data.table(name = sname[i],
                               masc = t_masc,
                               fem = t_fem,
                               nid = t_nid,
                               total = t_bici)
  return(DT2)
}) %>% data.table::rbindlist()
# --
# centro
# --
DT1 <- lapply(c(7,9),function(i){ # 7,9
  dtcen <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[i],startRow=8)
  dtcen1 <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[i+1],startRow=8)
  dtcen[is.na(dtcen)] <- 0
  dtcen1[is.na(dtcen1)] <- 0
  #dtcen1$BICI <- dtcen1$TOTAL
  #dtcen1 <- dtcen1[,-which(colnames(dtcen1) %in% "TOTAL")] 

  t_masc <- dtcen[,which(names(dtcen) %like% "masc")] %>% sum()
  t_fem <-  dtcen[,which(names(dtcen) %like% "fem")] %>% sum()
  t_nid <-  dtcen[,which(names(dtcen) %like% "nid")] %>% sum()
  t_bici <- dtcen[,which(names(dtcen) %like% "BICI")] %>% sum()
  # 1
  t_masc1 <- dtcen1[,which(names(dtcen1) %like% "masc")] %>% sum()
  t_fem1 <-  dtcen1[,which(names(dtcen1) %like% "fem")] %>% sum()
  t_nid1 <-  dtcen1[,which(names(dtcen1) %like% "nid")] %>% sum()
  if(i == 7){strr <- "TOTA"}else{strr <- "BICI"}
  t_bici1 <- dtcen1[,which(names(dtcen1) %like% strr)] %>% sum()
  
  DT1 <- data.table::data.table(name = sname[i],
                               masc = t_masc + t_masc1,
                               fem = t_fem + t_fem1,
                               nid = t_nid + t_nid1,
                               total = t_bici + t_bici1)
  return(DT1)
}) %>% data.table::rbindlist()

DTT <- rbind(DT1,DT2)

DTT[,p_masc := (100 * masc/total) %>% round(1)][,p_fem := (100 * fem/total) %>% round(1)]

DTT[,name := stringr::str_remove_all(name,"(centro)")]

data.table::fwrite(DTT,"output/genero.csv")
       