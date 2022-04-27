rm(list=ls())
library(readODS)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(data.table)
library(facetscales)
library(readr)

# read 2021----
dt2021 <- readr::read_rds("data/2021/contagem_2021.rds")
dt2021[1:5]
dt2021 <- dt2021[periodo == "Tarde",sum(total),by = veiculo]
dt2021 <- dt2021[veiculo %in% c("Bicicleta","Automóvel")]
dt2021[,Ano := 2021]
data.table::setnames(dt2021,"V1","Total")
data.table::setnames(dt2021,"veiculo","Veiculo")

# read previous data----
openxlsx::getSheetNames("data/2018/rep_argentina.xlsx")
rep_arg <- openxlsx::read.xlsx("data/2018/rep_argentina.xlsx", sheet = "Rep. Arg")

# Rbind ---
dtbind <- rbind(rep_arg,dt2021)
data.table::setDT(dtbind)
# ggplot2----
dtbind[,Ano_f := factor(x = Ano,levels = c(2016,2018,2019,2021)
                        ,labels = c(2016,2018,2019,2021))]
ggplot(dtbind) +
  geom_bar(aes(x = Ano_f,y = Total,fill = Veiculo),stat = "identity") +
  facet_grid(rows = vars(Veiculo),scales = "free_y")+
  geom_text(data = dtbind,
            aes(x = Ano_f, y = Total,label = Total
            ),vjust = +1.5,colour = "black",size = 3.5) +
  labs(x = NULL,y = "Total de veículos",fill = NULL,
       title = "Av. República Argentina X Av. Getúlio Vargas"
       ,caption = "Fluxo total no horário de pico da tarde (16:30 - 19:30), em ambos sentidos")+
  theme(axis.text.x = element_text(size = 9, angle = 0),
        legend.position = "none")

ggsave(filename =paste0("graphics/2021/historico_rep_arg.jpg"), scale = 1.2,
       width = 15,height = 10,units = "cm",dpi = "print")
