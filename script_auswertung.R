#Bibliothek
library(pdfsearch)
library(readr)
library(dplyr)
library(igraph)
library(stringr)

# Daten aus PDF lesen =================

data_signaturen <- read_csv("data_signaturen.csv", 
                            col_types = cols(data_signaturen = col_character()))
data_signaturen <- data_signaturen$data_signaturen
data_GR_Namen <- read_csv("GR_Namen.csv")

data_extrakt <- NULL

for (i in 1:length(data_signaturen)) {
  zwischenwert <- keyword_search(paste0("geschaefte/",data_signaturen[i],".pdf"), keyword = c(data_GR_Namen$Name1,data_GR_Namen$Name2), path = TRUE)
  zwischenwert$gr_anzahl <- dim(zwischenwert)[1]
  zwischenwert$signatur <- data_signaturen[i]
  data_extrakt <- rbind(data_extrakt,zwischenwert)
}

data_extrakt <- unique(data_extrakt)

write_csv(data_extrakt,"data_extrakt.csv")


# Matrix erstellen =============

data_extrakt <- select(data_extrakt,keyword,signatur,gr_anzahl)

data_network <- matrix(0,nrow = length(data_GR_Namen$Name1),ncol = length(data_GR_Namen$Name1), dimnames = list(data_GR_Namen$Name1,data_GR_Namen$Name1))
data_network_anzahlGR_mean <- matrix(0,nrow = length(data_GR_Namen$Name1),ncol = length(data_GR_Namen$Name1), dimnames = list(data_GR_Namen$Name1,data_GR_Namen$Name1))
data_network_anzahlGR_max <- matrix(0,nrow = length(data_GR_Namen$Name1),ncol = length(data_GR_Namen$Name1), dimnames = list(data_GR_Namen$Name1,data_GR_Namen$Name1))
data_network_anzahlGR_min <- matrix(0,nrow = length(data_GR_Namen$Name1),ncol = length(data_GR_Namen$Name1), dimnames = list(data_GR_Namen$Name1,data_GR_Namen$Name1))
data_network_anzahlGR_median <- matrix(0,nrow = length(data_GR_Namen$Name1),ncol = length(data_GR_Namen$Name1), dimnames = list(data_GR_Namen$Name1,data_GR_Namen$Name1))


for (i in 1:length(data_GR_Namen$Name1)) {
  data_extrakt_loop <- data_extrakt %>% filter(keyword %in% c(data_GR_Namen$Name1[i],data_GR_Namen$Name2[i]))
  
  if (dim(data_extrakt_loop)[1]>0){
      for (ii in 1:length(data_GR_Namen$Name1)) {
         data_extrakt_loop_loop <- data_extrakt %>% filter(signatur %in% data_extrakt_loop$signatur)
         zwischenwert <- data_extrakt_loop_loop %>% filter(keyword %in% c(data_GR_Namen$Name1[ii],data_GR_Namen$Name2[ii]))
         data_network[i,ii] <- dim(zwischenwert)[1]
         data_network_anzahlGR_mean[i,ii] <- mean(zwischenwert$gr_anzahl)
         data_network_anzahlGR_max[i,ii] <- max(zwischenwert$gr_anzahl)
         data_network_anzahlGR_min[i,ii] <- min(zwischenwert$gr_anzahl)
         data_network_anzahlGR_median[i,ii] <- median(zwischenwert$gr_anzahl)
         
      }
  }
  
}


# Netzwerk-Darstellung / igraph ==============
#data_network_save <- data_network

data_network_anzahlGR_min -> data_network

# nur Vorstösse mit weniger als 7 Unterzeichner:innen,
# Gewichtung anhand Anzahl Unterzeichner:innen 
data_network[data_network>6] <- 0
data_network[data_network=="Inf"] <- 0
data_network[data_network==1] <- 6
data_network[data_network==2] <- 5
data_network[data_network==3] <- 4
data_network[data_network==4] <- 3
data_network[data_network==5] <- 2
data_network[data_network==6] <- 1

# Zusätzliche Gewichtung anhand der Anzahl Vorstösse 
data_network_save -> data_network_zusatz
data_network_zusatz[data_network_zusatz>10] <- 10
data_network_zusatz <- data_network_zusatz/10
data_network_zusatz <- data_network_zusatz+1

data_network <- data_network_zusatz*data_network

# Zeilenumbruch im Namen
rownames(data_network) <- str_replace_all(colnames(data_network)," ","\n")
colnames(data_network) <- str_replace_all(colnames(data_network)," ","\n")

# Gewichtung anpassen für Darstelluung
data_network <- data_network/4

#write.csv(data_network, file="netzwerk.csv",row.names = colnames(data_network), col.names = colnames(data_network))

# Netzwerk erstellen   ============
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0)
  delete.vertices(graph, isolates)
}

network <- graph_from_adjacency_matrix(data_network , mode='upper', weighted = T, diag=F)

# Plot
network_plot <- delete.isolates(network)
#network_plot <- delete.edges(network_plot, which(E(network_plot)$weight <2))
cluster_louvain(network_plot) -> network_cluster
set.seed(20)
par(mar=c(0,0,0,0))
plot(network_cluster, network_plot, layout = layout_nicely, vertex.label.cex=0.7)

