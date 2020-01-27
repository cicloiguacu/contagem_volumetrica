# intro
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(data.table)
library(dplyr)
sname <- openxlsx::getSheetNames("data/2019/contagens2019.xlsx")


# centro
dtcen <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[9],startRow=8)
dtcen1 <- openxlsx::read.xlsx(xlsxFile = "data/2019/contagens2019.xlsx",sheet = sname[9+1],startRow=8)
dtcen2 <- rbind(dtcen,dtcen1)
dtcen2[is.na(dtcen2)] <- 0

t_masc <- dtcen2[,which(names(dtcen2) %like% "masc")] %>% sum()
t_masc
t_fem <-  dtcen2[,which(names(dtcen2) %like% "fem")] %>% sum()
t_fem
t_bici <- dtcen2[,which(names(dtcen2) %like% "BICI")] %>% sum()
t_bici

DT <- data.table:as.data.table("name" = sname[9],
                            "masc" = t_masc,
                            "fem" = t_fem,
                            "total" = t_bici)
sapply(1:length(names(dtcen)),function(i){which(names(dtcen)[i] %like% c("masc","fem","nid","BICI"))})
       