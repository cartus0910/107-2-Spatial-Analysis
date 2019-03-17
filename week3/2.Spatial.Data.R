
#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 2: Spatial Data Handling                      #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
# Source: Brunsdon and Comber(2015), Chapter 3          #
#                                                       #
# Date: 2019-03-04                                      #
#                                                       #
#########################################################


library(GISTools)
data(newhaven)
ls()
plot(roads)
plot(blocks)
block.attr<-data.frame(blocks)
head(block.attr)

########## 1. Plotting Spatial Objects ###########
par(mar = c(0,0,0,0)) 
plot(blocks)
plot(roads, add=TRUE, col="red")

plot(blocks)
plot(breach, col = "red", add = TRUE)

plot(blocks, lwd = 0.5, border = "grey50") 
plot(breach, col = "red", pch = 1, add = TRUE)

colors()

### Map Layout #### 
library(GISTools)
data(newhaven)
# plot spatial data
par(mar = c(0,0,2,0))
plot(blocks)
plot(breach,add=TRUE,col= 'red', pch = 1)
# embellish the map
locator()
map.scale(534750,152000,miles2ft(2), "Miles",4,0.5) 
north.arrow(533043.9,154617.4,miles2ft(0.2),col= 'lightblue') 
title('New Haven, CT')

?locator()

### Map Layout ### 
library(rgdal)
EPA.STN <- readOGR(dsn = "./data/SHP", layer = "EPA_STN1", encoding="unicode")
Popn.TWN <- readOGR(dsn = "./data/SHP", layer = "Popn_TWN2", encoding="unicode")
par(mar = c(0,2,2,2))
plot(Popn.TWN, col='lightgreen', border="grey", bg='aliceblue')
Popn.TWN.attr<-data.frame(Popn.TWN)
plot(EPA.STN,add=TRUE,col= 'blue', pch = 16)
map.scale(63030.22,2472112,100000, "50 km",2,1) 
north.arrow(49646.41,2534913,10000,col= 'lightblue') 
title('Taiwan EPA Stations')


### Adding Contexts (OpenStreetMaps as Background) ###

### Map Projection Transformation ###
library(rgdal);library(sp)
proj4string(Popn.TWN)
TWN.LongLat <- spTransform(Popn.TWN, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

?spTransform

### OpenStreepMap ###
Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre1.8.0_201/bin")
library(rJava)
library(OpenStreetMap)
# define upper left, lower right corners
ul <- as.vector(cbind(bbox(TWN.LongLat)[2,2], bbox(TWN.LongLat)[1,1])) # lat-long coord
lr <- as.vector(cbind(bbox(TWN.LongLat)[2,1], bbox(TWN.LongLat)[1,2])) # lat-long coord
# download the map tile
MyMap <- openmap(ul,lr,9, "esri-topo")
# now plot the layer and the backdrop
par(mar = c(0,0,0,0))
plot(MyMap, removeMargin=FALSE) 
plot(spTransform(EPA.STN, osm()), pch = 16, add = TRUE, col="red", cex=1.2)


########## 2. Mapping Spatial Data Attributes ###########

library(GISTools)
data(newhaven)
data.frame(blocks)
head(data.frame(blocks))
colnames(data.frame(blocks))
data.frame(blocks)$P_VACANT
blocks$P_VACANT
attach(data.frame(blocks))
par(mar = c(3,5,3,3))
hist(P_VACANT)

### Choropleth Mapping ###
par(mar = c(3,5,3,3))
choropleth(blocks, blocks$P_VACANT)
vacant.shades = auto.shading(blocks$P_VACANT)
vacant.shades
choro.legend(533000,161000,vacant.shades)

### Set Shading Argument (no. of intervals) ###
par(mar = c(0,5,0,3))
vacant.shades = auto.shading(blocks$P_VACANT,n=7)
# plot the map 
choropleth(blocks,blocks$P_VACANT,shading=vacant.shades) 
choro.legend(533000,161000,vacant.shades)

### Set Shading Argument (colors) ###
brewer.pal(5,'Blues')
vacant.shades = auto.shading(blocks$P_VACANT, cols=brewer.pal(7,"Greens"), n=7)
choropleth(blocks, blocks$P_VACANT,shading=vacant.shades)
choro.legend(533000,161000,vacant.shades)

### Set Shading Argument (cutters) ###
vacant.shades = auto.shading(blocks$P_VACANT, n=5, cols=brewer.pal(5,"Blues"), cutter=rangeCuts)
choropleth(blocks,blocks$P_VACANT,shading=vacant.shades)
choro.legend(533000,161000,vacant.shades)


