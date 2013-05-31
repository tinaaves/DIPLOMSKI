#Analiza nalaza prstenovanih ptica na Vranskom jezeru
#Izvor podataka D. Radoviæ


install.packages("xlsx")
library(xlsx)
   
   setwd( "D:\\Andreja\\ANALIZE\\Tina Kar")
   list.files(getwd())
   
   latlon <- CRS("+proj=longlat +datum=WGS84") #EPSG:4326
   
   
podaci.d <- read.xlsx("Vransko-nalazi svi - drugi dio.xlsx", as.data.frame=T, stringsAsFactors = F,  sheetName="svi_domaci", encoding="Latin-1" )#domaæi nalazi 


podaci.d <- podaci.d[-c(16, 19:21)]
names(podaci.d) <- c("Vrsta", "Centrala", "Serija", "Broj_prstena", "Starost", "Spol", "Datum_prstenovanja", "Mjesto prstenovanja", "Koordinata_prestenovanja", "Prstenovaè", "Datum_nalaza", "Mjesto_nalaza", "Koordinata_nalaza", "Prijavio", "Ostalo", "Udaljenost", "Smjer")
 
for (i in 1:(dim (podaci.d)[2])) {

podaci.d[,i] <- gsub("Å¡", "š", podaci.d[,i]) 
podaci.d[,i] <- gsub("Ä", "è", podaci.d[,i]) 
podaci.d[,i] <- gsub("Ä‘", "ð", podaci.d[,i]) 
podaci.d[,i] <- gsub("Å¾", "ž", podaci.d[,i]) 
podaci.d[,i] <- gsub("Å ", "Š", podaci.d[,i]) 
podaci.d[,i] <- gsub("Å¡", "ž", podaci.d[,i])
podaci.d[,i] <- gsub("ÄŒ", "È", podaci.d[,i]) 
podaci.d[,i] <- gsub("Ä‡", "æ", podaci.d[,i]) 
podaci.d[,i] <- gsub("Ä", "Ð", podaci.d[,i])
podaci.d[,i] <- gsub("ÄŒ", "È", podaci.d[,i])
}

podaci.d_nodupl <- podaci.d[!duplicated(podaci.d),]   #10 redaka u potpunosti identièni - pobrisani

#podaci.d[1795,]
  #podaci.d[which(c(podaci.d[,4]==19726)),]

podaci.d_nodupl_noloco <-  podaci.d_nodupl[which(c(!podaci.d_nodupl$Mjesto_nalaza=="loco")),]   #bez dupliciranih redaka i izbrisani loco - 718



write.xlsx (podaci.d_nodupl_noloco, "domaci_noloco_nodupl.xlsx")


 #strani
 
 podaci.s <- read.xlsx("Vransko-nalazi svi - drugi dio.xlsx", as.data.frame=T, stringsAsFactors = F,  sheetName="svi_strani", encoding="Latin-1" )#domaæi nalazi 

podaci.s <- podaci.s[-c(14:19)]
names(podaci.s) <- c("Vrsta", "Centrala", "Serija", "Broj_prstena", "Starost", "Spol", "Datum_prstenovanja", "Mjesto prstenovanja", "Koordinata_prestenovanja", "Prstenovaè", "Datum_nalaza", "Mjesto_nalaza", "Koordinata_nalaza") #, "Prijavio", "Ostalo", "Udaljenost", "Smjer")

#podaci.s <-podaci.s[-c(14, 17)] 
 
for (i in 1:(dim (podaci.s)[2])) {

podaci.s[,i] <- gsub("Å¡", "š", podaci.s[,i]) 
podaci.s[,i] <- gsub("Ä", "è", podaci.s[,i]) 
podaci.s[,i] <- gsub("Ä‘", "ð", podaci.s[,i]) 
podaci.s[,i] <- gsub("Å¾", "ž", podaci.s[,i]) 
podaci.s[,i] <- gsub("Å ", "Š", podaci.s[,i]) 
podaci.s[,i] <- gsub("Å¡", "ž", podaci.s[,i])
podaci.s[,i] <- gsub("ÄŒ", "È", podaci.s[,i]) 
podaci.s[,i] <- gsub("Ä‡", "æ", podaci.s[,i]) 
podaci.s[,i] <- gsub("Ä", "Ð", podaci.s[,i])
podaci.s[,i] <- gsub("ÄŒ", "È", podaci.s[,i])

}

 
podaci.s[,7] <- as.Date(podaci.s[,7], "%d.%m.%Y") 
podaci.s[,11] <- as.Date(podaci.s[,11], "%d.%m.%Y") 


podaci.s_nodupl <- podaci.s[!duplicated(podaci.s),]   #

podaci.s[619,]
  #podaci.d[which(c(podaci.d[,4]==19726)),]
  
 #podaci.s[which(c(podaci.s[,4]=="156294")),] 
  
 podaci.s_nodupl$rownames <- row.names(podaci.s_nodupl)

write.xlsx (podaci.s_nodupl, "strani_nodupl.xlsx")




#proba kml

   
 kml.nalazi<- read.xlsx("Vransko-nalazi svi - drugi dio.xlsx", as.data.frame=T, stringsAsFactors = F,  sheetName="prstenovani 1910-2009", encoding="Latin-1" )
 
 kml.nalazi <- kml.nalazi[-c(9:14, 20:25,  30, 33:35)] 
 
 names(kml.nalazi) <- c("Vrsta", "Centrala", "Serija", "Broj_prstena", "Starost", "Spol", "Datum_prstenovanja", "Mjesto prstenovanja", "y", "x", "Prstenovaè", "Datum_nalaza", "Mjesto_nalaza","y_n", "x_n", "Prijavio", "Ostalo", "Udaljenost", "Smjer")
 

 coordinates( kml.nalazi) <- ~ x_n + y_n
 proj4string( kml.nalazi) <- latlon
 
 writeOGR(kml.nalazi, "vransko_nalazi.kml", ".", driver= "KML", overwrite=T)
 

#eos



 


