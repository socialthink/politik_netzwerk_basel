library(jsonlite)
library(rvest)
library(xml2)
library(stringr)

library(readr)


# Funktion Webscrapping alle Vorstösse
sothi_download_file <- function(quelle,output_path){
  webseite <- xml2::read_html(quelle)
  seite <- webseite %>% html_element("#detail_table_geschaeft_dokumente tr:nth-child(1) td~ td+ td")
  filename <- seite %>% html_nodes("a")  %>%  html_attr('href')
  download.file(filename,output_path)
}



# Download Loop

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
