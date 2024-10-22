#### load libraries
library(sf)
library(tmap)
library(mapview)
library(leaflet)
library(tidyverse)
library(readxl)

## tettsteder
st_layers(dsn = "P:/22051000_egenutvikling_joachim_paul_topper/fremmedarter tettsteder/Befolkning_25833_Tettsteder2023_FGDB.gdb")
tett <- st_read("P:/22051000_egenutvikling_joachim_paul_topper/fremmedarter tettsteder/Befolkning_25833_Tettsteder2023_FGDB.gdb", layer="tettsted")

tm_shape(tett) +
  tm_fill('tettstednavn', labels="", title="", legend.show = TRUE) + 
  tm_borders()


## fremmedarter
fa <- read_excel("P:/22051000_egenutvikling_joachim_paul_topper/fremmedarter tettsteder/fa.xlsx",sheet="fa")

names(fa)

fa <- fa[fa$Antall!="< 1 % dekning",]
fa$Antall <- as.numeric(fa$Antall)
summary(fa$Antall)
fa$Antall2 <- fa$Antall

fa[is.na(fa$Antall),'Antall2'] <- median(fa$Antall,na.rm=T)

fa$Antall3 <- fa$Antall
fa[is.na(fa$Antall),'Antall3'] <- sample(fa$Antall[!is.na(fa$Antall)],sum(is.na(fa$Antall)),replace=T)

fa <- st_as_sf(fa, coords=c("Øst","Nord"),crs=st_crs(tett),remove=FALSE)

plot(fa)

tm_shape(tett) +
  tm_shape(fa)

e <- st_bbox(c(xmin = -45000, xmax = -35000, ymax = 6736000, ymin = 6730000), crs = st_crs(tett))

tm_shape(tett, bbox = e) +
  tm_fill('lokalid', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_shape(fa) +
  tm_dots('Kategori',midpoint=NA, palette=tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE), scale=1, legend.show = TRUE)


mapview(tett["lokalid"]) +
  st_geometry(fa["Kategori"])

leaflet(tett) %>%
  addTiles() %>%
  addPolygons(fillOpacity = 1,
              weight = 1,
              fillColor = ~pal(lokalid)) %>%
  addCircleMarkers(data = fa["Kategori"])

st_distance(fa, tett)

fa2 <- fa[tett,]

tm_shape(tett, bbox = e) +
  tm_fill('lokalid', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_shape(fa2) +
  tm_dots('Kategori',midpoint=NA, palette=tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE), scale=1, legend.show = TRUE)


tett$pt_count <- lengths(st_intersects(tett, fa))

#tett$AntallSum <- sum(st_intersects(tett, fa))


hist(tett$pt_count, breaks=50)

tett$area <- st_area(tett)
tett$ratio <- tett$pt_count/tett$area
with(tett,plot(area,ratio))

tett2 <- tett %>%
  filter(tettsednavn %in% c("Bergen","Askøy","Øygarden"))