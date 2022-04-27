# gender peak

rm(list=ls())
library(readODS)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(data.table)
library(facetscales)

sname <- readODS::list_ods_sheets("data/2021/Contagem 2021.ods")
sname

# READ 2021-----
## centro----
dtcen <- readODS::read_ods(path = "data/2021/Contagem 2021.ods"
                           ,sheet = "23.11 arg + get (centro)",skip = 6)
dtcen
dtcen[is.na(dtcen)] <- 0
dtcen
dtcen <- data.table::setDT(dtcen)
data.table::setnames(dtcen,"","Período:.manhã")

tempo <- stringr::str_split_fixed(dtcen$`Período:.manhã`,"-",2)[,1] %>% 
  data.table::as.ITime() %>% as.POSIXct() %>% format("%H:%M")
nomes <- c("Via Calma","Contra-mão","Canaleta")
dtcen[1:12,periodo := "Manhã"]
dtcen[13:24,periodo := "Tarde"]

dtcen <- data.table::melt.data.table(data = dtcen
                                     ,measure.vars = c("can_masc","can_masc_del"
                                                       ,"can_fem","can_nid","cm_masc"       
                                                       ,"cm_fem","cm_nid","vc_masc"
                                                       ,"vc_fem","vc_nid"
                                                       ,"AUTOMÓVEL","MOTO"
                                                       ,"CAMINHÃO (+ 2 EIXOS)"
                                                       ,"CAMINHÃO (2 EIXOS)"
                                                       ,"OUTRO")
                                     ,id.vars = c("periodo", "Período:.manhã")
                                     ,value.name = "total")
dtcen$variable %>% unique()
dtcen[variable %in% "AUTOMÓVEL",veiculo := "Automóvel"]
dtcen[variable %like% "MOTO",veiculo := "Moto"]
dtcen[variable %like% "CAMINHÃO",veiculo := "Caminhão"]
dtcen[variable %like% "OUTRO",veiculo := "Outro"]
dtcen[is.na(veiculo),veiculo := "Bicicleta"]

dtcen[variable %like% "cm_",local := "Contra-mão"]
dtcen[variable %like% "vc_",local := "Via Calma"]
dtcen[variable %like% "can",local := "Canaleta"]
dtcen[is.na(local),local := "Via Calma"]

dtcen[variable %like% "masc",gender := "Masculino"]
dtcen[variable %like% "fem",gender := "Feminino"]
dtcen[variable %like% "nid",gender := "N/ID"]
dtcen[is.na(gender),gender := NA]

dtcen[,way := "Centro"]
data.table::setnames(dtcen,"Período:.manhã","horario")

## bairro----
dtbai <- readODS::read_ods(path = "data/2021/Contagem 2021.ods",sheet = "23.11 arg + get (bairro)",skip = 7)
dtbai[is.na(dtbai)] <- 0
dtbai <- setDT(dtbai)
data.table::setnames(dtbai,"","Período:.manhã")

tempo <- stringr::str_split_fixed(dtbai$`Período:.manhã`,"-",2)[,1] %>% 
  data.table::as.ITime() %>% as.POSIXct() %>% format("%H:%M")
nomes <- c("Via Calma","Contra-mão","Canaleta")
dtbai[1:12,periodo := "Manhã"]
dtbai[13:24,periodo := "Tarde"]

dtbai <- data.table::melt.data.table(data = dtbai
                                     ,measure.vars = c("can_masc","can_masc_del"
                                                       ,"can_fem","can_nid","cm_masc"       
                                                       ,"cm_fem","cm_nid","vc_masc"
                                                       ,"vc_fem","vc_nid"
                                                       ,"AUTOMÓVEL","MOTO"
                                                       ,"CAMINHÃO (+ 2 EIXOS)"
                                                       ,"CAMINHÃO (2 EIXOS)"
                                                       ,"OUTRO")
                                     ,id.vars = c("periodo", "Período:.manhã")
                                     ,value.name = "total")
dtbai$variable %>% unique()
dtbai[variable %in% "AUTOMÓVEL",veiculo := "Automóvel"]
dtbai[variable %like% "MOTO",veiculo := "Moto"]
dtbai[variable %like% "CAMINHÃO",veiculo := "Caminhão"]
dtbai[variable %like% "OUTRO",veiculo := "Outro"]
dtbai[is.na(veiculo),veiculo := "Bicicleta"]

dtbai[variable %like% "cm_",local := "Contra-mão"]
dtbai[variable %like% "vc_",local := "Via Calma"]
dtbai[variable %like% "can",local := "Canaleta"]
dtbai[is.na(local),local := "Via Calma"]

dtbai[variable %like% "masc",gender := "Masculino"]
dtbai[variable %like% "fem",gender := "Feminino"]
dtbai[variable %like% "nid",gender := "N/ID"]
dtbai[is.na(gender),gender := NA]
dtbai[,way := "Bairro"]
data.table::setnames(dtbai,"Período:.manhã","horario")

## rbind -----

names(dtcen)
names(dtbai)
dtbind <- rbind(dtcen,dtbai)

readr::write_rds(dtbind,"data/2021/contagem_2021.rds")
