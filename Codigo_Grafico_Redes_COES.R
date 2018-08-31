
#GESTIONAR ESPACIO DE TRABAJO
rm(list = ls())


#CARGAR PAQUETES
library(GGally)
library(ggplot2)
library(network)
library(readxl)
library(sna)
library(splitstackshape)


#IMPORTAR BASES DE DATOS
#Se cargan las matrices de adyacencia (cada hoja es un año). Se fija un rango, equivalente a ignorar la primera columna 
#(que corresponde a los nombres de los investigadores).

coes_2014 <- read_excel(path  = "Datos/coes_2014.xlsx", range = "B1:AF32")
coes_2015 <- read_excel(path  = "Datos/coes_2015.xlsx", range = "B1:AS45")
coes_2016 <- read_excel(path  = "Datos/coes_2016.xlsx", range = "B1:AY51")
coes_2017 <- read_excel(path  = "Datos/coes_2017.xlsx", range = "B1:BN66")
coes_2018 <- read_excel(path  = "Datos/coes_2018.xlsx", range = "B1:BQ69")


#CONVERTIR EN MATRICES
coes_2014a  <- as.matrix(coes_2014)
coes_2015a  <- as.matrix(coes_2015)
coes_2016a  <- as.matrix(coes_2016)
coes_2017a  <- as.matrix(coes_2017)
coes_2018a  <- as.matrix(coes_2018)


#EDITAR NOMBRES DE INVESTIGADORES
names_2014 <- sub(".*? (.+)", "\\1",sub(".*? (.+)", "\\1", colnames(coes_2014a)))
names_2015 <- sub(".*? (.+)", "\\1",sub(".*? (.+)", "\\1", colnames(coes_2015a)))
names_2016 <- sub(".*? (.+)", "\\1",sub(".*? (.+)", "\\1", colnames(coes_2016a)))
names_2017 <- sub(".*? (.+)", "\\1",sub(".*? (.+)", "\\1", colnames(coes_2017a)))
names_2018 <- sub(".*? (.+)", "\\1",sub(".*? (.+)", "\\1", colnames(coes_2018a)))

#2014
names_2014[24] <- "De Tezanos"
names_2014[25] <- "González, P."
names_2014[29] <- "González, R."
colnames(coes_2014a) <- rownames(coes_2014a) <- names_2014

#2015
names_2015[35] <- "De Tezanos"
names_2015[36] <- "González, P."
names_2015[42] <- "González, R."
colnames(coes_2015a) <- rownames(coes_2015a) <- names_2015

#2016
names_2016[40] <- "De Tezanos"
names_2016[47] <- "González, R."
colnames(coes_2016a) <- rownames(coes_2016a) <- names_2016

#2017
names_2017[25] <- "Delamaza"
names_2017[44] <- "Pérez, M."
names_2017[50] <- "De Tezanos"
names_2017[51] <- "Pérez, P."
names_2017[60] <- "González, R."
colnames(coes_2017a) <- rownames(coes_2017a) <- names_2017

#2018
names_2018[11] <- "Berger"
names_2018[26] <- "Delamaza"
names_2018[45] <- "Pérez, M."
names_2018[51] <- "De Tezanos"
names_2018[52] <- "Pérez, P."
names_2018[63] <- "González, R."
colnames(coes_2018a) <- rownames(coes_2018a) <- names_2018

#LINEAS DE INVESTIGACION 2014
lineas_2014 <- c("L3","L2","L3","L3","L1","L4","L3","L1","L2","L1","L2","L2","L4","L4","L3","L2","L1","L4","L4",
                 "L2","L1","L3","L3","L2","L2","L4","L2","L3","L2","L3","L4")
labels_2014 <- ifelse(lineas_2014=="L1","Dimensiones Socioeconómicas del Conflicto",
                      ifelse(lineas_2014=="L2", "Interacciones Grupales e Individuales",
                             ifelse(lineas_2014=="L3", "Conflicto Político y Social",
                                    "Geografías del Conflicto")))

#LINEAS DE INVESTIGACION 2015
lineas_2015 <-  c("L3","L2","L3","L3","L1","L3","L4","L1","L1","L2","L2","L4","L1","L4","L1","L2","L2",
                  "L4","L3","L3","L2","L2","L1","L4","L4","L2","L4","L1","L3","L3","L2","L4","L1","L3",
                  "L2","L2","L1","L4","L2","L3","L2","L2","L3","L4")
labels_2015 <- ifelse(lineas_2015=="L1","Dimensiones Socioeconómicas del Conflicto",
                      ifelse(lineas_2015=="L2", "Interacciones Grupales e Individuales",
                             ifelse(lineas_2015=="L3", "Conflicto Político y Social",
                                    "Geografías del Conflicto")))

#LINEAS DE INVESTIGACION 2016
lineas_2016 <- c("L1","L3","L2","L3","L3","L1","L4","L1","L3","L1","L2","L1","L2","L4","L1","L4","L1",
                 "L2","L2","L4","L1","L3","L3","L4","L2","L2","L1","L4","L3","L4","L3","L4","L1","L3",
                 "L3","L2","L4","L1","L3","L2","L1","L4","L2","L3","L2","L4","L2","L3","L1","L4")
labels_2016 <- ifelse(lineas_2016=="L1","Dimensiones Socioeconómicas del Conflicto",
                      ifelse(lineas_2016=="L2", "Interacciones Grupales e Individuales",
                             ifelse(lineas_2016=="L3", "Conflicto Político y Social",
                                    "Geografías del Conflicto")))

#LINEAS DE INVESTIGACION 2017
lineas_2017 <- c("L1","L2","L3","L2","L3","L4","L4","L3","L3","L1","L4","L3","L1","L3","L1","L2","L1",
                 "L2","L4","L1","L4","L1","L1","L2","L4","L3","L4","L1","L3","L3","L2","L2","L1","L4","L3",
                 "L2","L4","L4","L3","L4","L1","L3","L3","L4","L1","L2","L4","L1","L3","L2","L1","L1","L3",
                 "L1","L4","L2","L3","L2","L4","L2","L1","L3","L3","L1","L4")
labels_2017 <- ifelse(lineas_2017=="L1","Dimensiones Socioeconómicas del Conflicto",
                      ifelse(lineas_2017=="L2", "Interacciones Grupales e Individuales",
                             ifelse(lineas_2017=="L3", "Conflicto Político y Social",
                                    "Geografías del Conflicto")))

#LINEAS DE INVESTIGACION 2018
lineas_2018 <- c("L1","L2","L3","L2","L3","L4","L4","L3","L3","L1","L2","L4","L3","L1","L2","L3","L1",
                 "L2","L1","L2","L4","L4","L1","L1","L2","L4","L3","L4","L1","L3","L2","L2","L1","L1","L4",
                 "L3","L2","L2","L4","L4","L3","L4","L1","L3","L4","L1","L2","L4","L1","L3","L2","L1","L1",
                 "L3","L1","L4","L2","L3","L2","L4","L4","L4","L2","L1","L3","L3","L1","L4")
labels_2018 <- ifelse(lineas_2018=="L1","Dimensiones Socioeconómicas del Conflicto",
                      ifelse(lineas_2018=="L2", "Interacciones Grupales e Individuales",
                             ifelse(lineas_2018=="L3", "Conflicto Político y Social",
                                    "Geografías del Conflicto")))


#GENERAR OBJETOS NETWORK
net_2014 <- network(coes_2014a, directed = FALSE)
net_2015 <- network(coes_2015a, directed = FALSE)
net_2016 <- network(coes_2016a, directed = FALSE)
net_2017 <- network(coes_2017a, directed = FALSE)
net_2018 <- network(coes_2018a, directed = FALSE)


#AGREGAR ATRIBUTOS
net_2014 %v% "Name" = names_2014; net_2014 %v% "Label" = lineas_2014; net_2014 %v% "Línea" = labels_2014
net_2015 %v% "Name" = names_2015; net_2015 %v% "Label" = lineas_2015; net_2015 %v% "Línea" = labels_2015
net_2016 %v% "Name" = names_2016; net_2016 %v% "Label" = lineas_2016; net_2016 %v% "Línea" = labels_2016
net_2017 %v% "Name" = names_2017; net_2017 %v% "Label" = lineas_2017; net_2017 %v% "Línea" = labels_2017
net_2018 %v% "Name" = names_2018; net_2018 %v% "Label" = lineas_2018; net_2018 %v% "Línea" = labels_2018


#GRAFICAR REDES
set.seed(10052016)
ggnet2(net_2014, size = 6, mode = "fruchtermanreingold",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2014", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure_01.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)


set.seed(10052016)
ggnet2(net_2015, size = 6, mode = "fruchtermanreingold",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2015", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure_02.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)

set.seed(10052016)
ggnet2(net_2016, size = 6, mode = "fruchtermanreingold",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2016", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure_03.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)

set.seed(10052016)
ggnet2(net_2017, size = 6, mode = "fruchtermanreingold",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2017", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure_04.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)

set.seed(10052016)
ggnet2(net_2018, size = 6, mode = "fruchtermanreingold",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2018", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure_05.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)

######################################################################################################################################
set.seed(10052016)
ggnet2(net_2014, size = 6, mode = "circle",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2014", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure1.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)

set.seed(10052016)
ggnet2(net_2015, size = 6, mode = "circle",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2015", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure2.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)

set.seed(10052016)
ggnet2(net_2016, size = 6, mode = "circle",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2016", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure3.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)

set.seed(10052016)
ggnet2(net_2017, size = 6, mode = "circle",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2017", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure4.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)

set.seed(10052016)
ggnet2(net_2018, size = 6, mode = "circle",  shape = "Línea", color = "Línea", edge.size = 0.5,
       palette = "Set1", label.size = 5, legend.position = "bottom") + 
  geom_text(aes(label = label),position = position_nudge(y = +0.03)) + ggthemes::theme_fivethirtyeight() +
  scale_shape_manual(values=c(15,16,17,18), name = "Línea") + theme(legend.box.background = element_rect(colour = "black")) +
  labs(title = "COES 2018", caption = "Fuente: Información interna Centro COES, 2014-2018.")
ggsave(file = "Figure5.png",
       path = "/Users/benjaminmunozrojas/pCloud Sync/7_COES/5_Otros/Redes_COES",
       device = "png", dpi=1200, scale = 3, units = "cm" , width = 12, height = 8)


#circle
#circrand
#kamadakawai


#theme_economist
#theme_fivethirtyeight
#theme_hc
#theme_map
#theme_tufte
#toupper(substr(color, 1, 1))), color = "white", fontface = "bold"
            

ggnet2(net_2015, size = 5, mode = "fruchtermanreingold", label = "Name", shape = "Línea", color = "Línea")
ggnet2(net_2016, size = 5, mode = "fruchtermanreingold", label = "Name", shape = "Línea", color = "Línea")
ggnet2(net_2017, size = 5, mode = "fruchtermanreingold", label = "Name", shape = "Línea", color = "Línea")
ggnet2(net_2018, size = 5, mode = "fruchtermanreingold", label = "Name", shape = "Línea", color = "Línea")



rm(coes_2014, coes_2015, coes_2016, coes_2017, coes_2018) #remover versiones originales
rm(lineas_2014, lineas_2015, lineas_2016, lineas_2017, lineas_2018) #remover lineas
rm(labels_2014, labels_2015, labels_2016, labels_2017, labels_2018) #remover labels
rm(names_2014, names_2015, names_2016, names_2017, names_2018)      #remover nombres

#network.vertex.names(net_2014) <- names_2014a
