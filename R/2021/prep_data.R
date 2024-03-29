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
data.table::setnames(dtcen,"","Per�odo:.manh�")

tempo <- stringr::str_split_fixed(dtcen$`Per�odo:.manh�`,"-",2)[,1] %>% 
  data.table::as.ITime() %>% as.POSIXct() %>% format("%H:%M")
nomes <- c("Via Calma","Contra-m�o","Canaleta")
dtcen[1:12,periodo := "Manh�"]
dtcen[13:24,periodo := "Tarde"]

dtcen <- data.table::melt.data.table(data = dtcen
                                     ,measure.vars = c("can_masc","can_masc_del"
                                                       ,"can_fem","can_nid","cm_masc"       
                                                       ,"cm_fem","cm_nid","vc_masc"
                                                       ,"vc_fem","vc_nid"
                                                       ,"AUTOM�VEL","MOTO"
                                                       ,"CAMINH�O (+ 2 EIXOS)"
                                                       ,"CAMINH�O (2 EIXOS)"
                                                       ,"OUTRO")
                                     ,id.vars = c("periodo", "Per�odo:.manh�")
                                     ,value.name = "total")
dtcen$variable %>% unique()
dtcen[variable %in% "AUTOM�VEL",veiculo := "Autom�vel"]
dtcen[variable %like% "MOTO",veiculo := "Moto"]
dtcen[variable %like% "CAMINH�O",veiculo := "Caminh�o"]
dtcen[variable %like% "OUTRO",veiculo := "Outro"]
dtcen[is.na(veiculo),veiculo := "Bicicleta"]

dtcen[variable %like% "cm_",local := "Contra-m�o"]
dtcen[variable %like% "vc_",local := "Via Calma"]
dtcen[variable %like% "can",local := "Canaleta"]
dtcen[is.na(local),local := "Via Calma"]

dtcen[variable %like% "masc",gender := "Masculino"]
dtcen[variable %like% "fem",gender := "Feminino"]
dtcen[variable %like% "nid",gender := "N/ID"]
dtcen[is.na(gender),gender := NA]

dtcen[,way := "Centro"]
data.table::setnames(dtcen,"Per�odo:.manh�","horario")

## bairro----
dtbai <- readODS::read_ods(path = "data/2021/Contagem 2021.ods",sheet = "23.11 arg + get (bairro)",skip = 7)
dtbai[is.na(dtbai)] <- 0
dtbai <- setDT(dtbai)
data.table::setnames(dtbai,"","Per�odo:.manh�")

tempo <- stringr::str_split_fixed(dtbai$`Per�odo:.manh�`,"-",2)[,1] %>% 
  data.table::as.ITime() %>% as.POSIXct() %>% format("%H:%M")
nomes <- c("Via Calma","Contra-m�o","Canaleta")
dtbai[1:12,periodo := "Manh�"]
dtbai[13:24,periodo := "Tarde"]

dtbai <- data.table::melt.data.table(data = dtbai
                                     ,measure.vars = c("can_masc","can_masc_del"
                                                       ,"can_fem","can_nid","cm_masc"       
                                                       ,"cm_fem","cm_nid","vc_masc"
                                                       ,"vc_fem","vc_nid"
                                                       ,"AUTOM�VEL","MOTO"
                                                       ,"CAMINH�O (+ 2 EIXOS)"
                                                       ,"CAMINH�O (2 EIXOS)"
                                                       ,"OUTRO")
                                     ,id.vars = c("periodo", "Per�odo:.manh�")
                                     ,value.name = "total")
dtbai$variable %>% unique()
dtbai[variable %in% "AUTOM�VEL",veiculo := "Autom�vel"]
dtbai[variable %like% "MOTO",veiculo := "Moto"]
dtbai[variable %like% "CAMINH�O",veiculo := "Caminh�o"]
dtbai[variable %like% "OUTRO",veiculo := "Outro"]
dtbai[is.na(veiculo),veiculo := "Bicicleta"]

dtbai[variable %like% "cm_",local := "Contra-m�o"]
dtbai[variable %like% "vc_",local := "Via Calma"]
dtbai[variable %like% "can",local := "Canaleta"]
dtbai[is.na(local),local := "Via Calma"]

dtbai[variable %like% "masc",gender := "Masculino"]
dtbai[variable %like% "fem",gender := "Feminino"]
dtbai[variable %like% "nid",gender := "N/ID"]
dtbai[is.na(gender),gender := NA]
dtbai[,way := "Bairro"]
data.table::setnames(dtbai,"Per�odo:.manh�","horario")

## rbind -----

names(dtcen)
names(dtbai)
dtbind <- rbind(dtcen,dtbai)

readr::write_rds(dtbind,"data/2021/contagem_2021.rds")
