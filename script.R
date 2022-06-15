#Bibliothek
library(pdfsearch)
library(jsonlite)
library(rvest)
library(xml2)
library(stringr)

library(readr)
library(dplyr)

library(igraph)


# Webscrapping alle Vorstösse
sothi_download_file <- function(quelle,output_path){
webseite <- xml2::read_html(quelle)
seite <- webseite %>% html_element("#detail_table_geschaeft_dokumente tr:nth-child(1) td~ td+ td")
filename <- seite %>% html_nodes("a")  %>%  html_attr('href')
download.file(filename,output_path)
}



## Schlaufen =======

# Download

geschaefte <- read_delim("geschaefte.csv", 
                  delim = ";", escape_double = FALSE, col_types = cols(Signatur = col_character()), 
                  trim_ws = TRUE)

anzahl <- dim(geschaefte)[1]

data_signaturen <- NULL

for (i in 1:anzahl) {
  sothi_download_file(geschaefte$Geschäft[i],paste0("geschaefte/",geschaefte$Signatur[i],".pdf"))
  data_signaturen <- c(data_signaturen,geschaefte$Signatur[i])
}
write_csv(as.data.frame(data_signaturen),"data_signaturen.csv")


# Create Network

data_signaturen <- read_csv("data_signaturen.csv", 
                            col_types = cols(data_signaturen = col_character()))
data_signaturen <- data_signaturen$data_signaturen
data_GR_Namen <- read_csv("GR_Namen.csv")

data_extrakt <- NULL

for (i in 1:length(data_signaturen)) {
  zwischenwert <- keyword_search(paste0("geschaefte/",data_signaturen[i],".pdf"), keyword = data_GR_Namen$Suchbegriff, path = TRUE)
  zwischenwert$signatur <- data_signaturen[i]
  data_extrakt <- rbind(data_extrakt,zwischenwert)
}

data_extrakt <- select(data_extrakt,keyword,signatur)

data_network <- matrix(0,nrow = length(data_GR_Namen$Suchbegriff),ncol = length(data_GR_Namen$Suchbegriff), dimnames = list(data_GR_Namen$Suchbegriff,data_GR_Namen$Suchbegriff))

for (i in 1:length(data_GR_Namen$Suchbegriff)) {
  data_extrakt_loop <- data_extrakt %>% filter(keyword==data_GR_Namen$Suchbegriff[i])
  
  if (dim(data_extrakt_loop)[1]>0){
      for (ii in 1:length(data_GR_Namen$Suchbegriff)) {
         data_extrakt_loop_loop <- data_extrakt %>% filter(signatur %in% data_extrakt_loop$signatur)
        data_network[i,ii] <- dim(data_extrakt_loop_loop %>% filter(keyword==data_GR_Namen$Suchbegriff[ii]))[1]
      }
  }
  
}


# Netzwerk erstellen   ======
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0)
  delete.vertices(graph, isolates)
}
# igraph
network <- graph_from_adjacency_matrix(data_network , mode='upper', weighted = T, diag=F)

# Plot
network_plot <- delete.isolates(network)
#network_plot <- delete.edges(network_plot, which(E(network_plot)$weight <2))
cluster_louvain(network_plot) -> network_cluster
set.seed(20)
par(mar=c(0,0,0,0))
plot(network_cluster, network_plot, vertex.label.cex=0.6)

# weitere Cluster Plot
cluster_walktrap(network_plot) -> network_cluster
set.seed(20)
par(mar=c(0,0,0,0))
plot(network_cluster, network_plot, vertex.label.cex=0.6)
