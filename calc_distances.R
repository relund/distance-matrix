## Load zips
options(stringsAsFactors = F)
zip<-read.csv2("zip_jutland.csv")
head(zip)
dim(zip)

## Generate distance matrix using Google Maps
library(ggmap)
set<-1:5   # areas under considration
zip[set,]
distanceMat<-matrix(NA, nrow=length(set), ncol = length(set))
for(i in 1:length(set)) {
   for(j in 1:length(set)) {
      if (i==j) distanceMat[i,j] <- 0
      else distanceMat[i,j] <- mapdist(paste(zip$Zip[set[i]],", Denmark", sep=""),
                                       paste(zip$Zip[set[j]], ", Denmark", sep=""),
                                       mode = 'driving', output = 'simple')$km
   }
}
colnames(distanceMat)<-zip$Zip[set]
rownames(distanceMat)<-zip$Zip[set]
distanceMat
library(reshape2)
dat<-melt(distanceMat)
colnames(dat)<-c("from","to","km")
head(dat)


## Generate using Bing maps
library(taRifx.geo)
options(BingMapsKey="AmMDNdwXJoLY0o9OOH4hy0h5CIhhJMi6_iSX9aY5rU6gEX_So9eapfVpoQfMvk8_")
set<-1:5   # areas under considration
zip[set,]
distanceMatBing<-matrix(NA, nrow=length(set), ncol = length(set))
for (trys in 1:10) {   # number of times we try to connect
   for(i in set) {
      for(j in set) {
         if (i>j) {distanceMatBing[i,j] <- distanceMatBing[j,i]; next}   # assume symetric distances
         if (!is.na(distanceMatBing[i,j])) next   # value already calculated
         if (i==j) {distanceMatBing[i,j] <- 0; next}
         result <- try(georoute( paste(c(zip$Zip[i], zip$Zip[j]), " Danmark", sep=""), returntype="distance",  service="bing" ) )
         if (class(result) != "try-error") {
            distanceMatBing[i,j] <- result[1,1]
         }
      }
   }
}
colnames(distanceMatBing)<-zip$Zip[set]
rownames(distanceMatBing)<-zip$Zip[set]
distanceMatBing
save(distanceMatBing, file = "distanceMatBing.RData")
dat<-melt(distanceMatBing)
colnames(dat)<-c("from","to","km")
head(dat)
write.csv2(dat, "distances_jutland.csv", row.names = F)

