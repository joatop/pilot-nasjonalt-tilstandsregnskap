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

fa <- fa %>%
  mutate(Aar = substr(Funndato, nchar(Funndato)-3, nchar(Funndato)) ) %>%
  mutate_at('Aar', as.numeric)

fa$Aar

fa2 <- fa %>%
  filter(Aar > 1999) %>%
  filter(Kategori != 'Lav risiko (LO)')

fa2 <- fa2 %>%
  mutate(Antall = recode(Antall,
                         '< 1 % dekning' = '',
                         'o: present' = '') ) %>%
  mutate_at('Antall', as.numeric)

fa2 <- fa2 %>%
  mutate(Antall_noNA=replace_na(Antall,0.001))

summary(fa2$Antall)
summary(fa2$Antall_noNA)


#fa$Antall2 <- fa$Antall
#fa[is.na(fa$Antall),'Antall2'] <- median(fa$Antall,na.rm=T)
#fa$Antall3 <- fa$Antall
#fa[is.na(fa$Antall),'Antall3'] <- sample(fa$Antall[!is.na(fa$Antall)],sum(is.na(fa$Antall)),replace=T)

nrow(fa2)/nrow(fa2[fa2$Antall_noNA>0.1,])
nrow(fa2)/nrow(fa2[!is.na(fa2$Antall),])

fa2 <- st_as_sf(fa2, coords=c("Øst","Nord"),crs=st_crs(tett),remove=FALSE)

plot(fa2)

tm_shape(tett) +
  tm_fill('lokalid', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_shape(fa2) +
  tm_dots('Kategori',midpoint=NA, palette=tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE), scale=1, legend.show = TRUE)


e <- st_bbox(c(xmin = -45000, xmax = -35000, ymax = 6736000, ymin = 6730000), crs = st_crs(tett))

tm_shape(tett, bbox = e) +
  tm_fill('lokalid', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_shape(fa2) +
  tm_dots('Kategori',midpoint=NA, palette=tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE), scale=1, legend.show = TRUE)


mapview(tett["lokalid"]) +
  st_geometry(fa2["Kategori"])

leaflet(tett) %>%
  addTiles() %>%
  addPolygons(fillOpacity = 1,
              weight = 1,
              fillColor = ~pal(lokalid)) %>%
  addCircleMarkers(data = fa["Kategori"])


fa2 <- fa2[tett,]

tm_shape(tett) +
  tm_fill('lokalid', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_shape(fa2) +
  tm_dots('Kategori',midpoint=NA, palette=tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE), scale=1, legend.show = TRUE)


tm_shape(tett, bbox = e) +
  tm_fill('lokalid', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_shape(fa2) +
  tm_dots('Kategori',midpoint=NA, palette=tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE), scale=1, legend.show = TRUE)


tett$fa_count <- lengths(st_intersects(tett, fa2))
fa.noNA <- fa2 %>%
  filter(!is.na(Antall))
tett$fa_noNA <- lengths(st_intersects(tett, fa.noNA))
tett <- tett %>%
  mutate(fa_noNA_ratio = fa_count/fa_noNA)
tett$fa_noNA_ratio 

#tett$AntallSum <- sum(st_intersects(tett, fa))

fa_sum <- fa2 %>%
  mutate(
    polygon_id = as.character(st_intersects(geometry, tett))
  ) %>% 
  group_by(polygon_id) %>% 
  summarise(fa_sum_polygon = sum(Antall_noNA,na.rm=T)) 

tett <- tett %>% 
  st_join(fa_sum)

tett <- tett %>%
  mutate(fa_sumest_polygon = fa_sum_polygon*fa_noNA_ratio)

summary(tett$fa_sumest_polygon)

tett.na <- tett %>% 
  mutate_if(is.numeric, function(fa_sumest_polygon) ifelse(is.infinite(fa_sumest_polygon), NA, fa_sumest_polygon))
tett.na_median <- median(tett.na$fa_sumest_polygon, na.rm=T)

tett <- tett %>% 
  mutate_if(is.numeric, function(fa_sumest_polygon) ifelse(is.infinite(fa_sumest_polygon), tett.na_median, fa_sumest_polygon))
#tett <- tett %>%
#  mutate(fa_sumest_polygon=replace_na(fa_sumest_polygon,0))
rm(tett.na)
rm(tett.na_median)

summary(tett$fa_sumest_polygon)

tett$area <- st_area(tett)

with(tett,plot(area,fa_sumest_polygon))

tett$ratio_m <- tett$fa_sumest_polygon/tett$area
tett$ratio_km <- tett$ratio_m * 1000000

summary(tett$ratio_m)
hist(tett$ratio_m,breaks=100)

tett %>%
  filter(!is.na(tett$ratio_m)) %>%
  summarize(areaWeightedMean = sum(ratio_m*area,na.rm=T)/sum(area,na.rm=T) )

sum(tett$ratio_m*tett$area,na.rm=T)/sum(tett$area,na.rm=T)

with(tett,plot(area,ratio_m))

tett.Bergen <- tett %>%
  filter(tettstednavn %in% c("Bergen"))
sum(tett.Bergen$ratio_m*tett.Bergen$area)/sum(tett.Bergen$area)

e <- st_bbox(c(xmin = -54654.02, xmax = 10000, ymax = 6774010.57, ymin = 6700273.53), crs = st_crs(tett))


tm_shape(tett, bbox = e) +
  tm_graticules() + 
  tm_layout(main.title='Alien species in the Bergen area',main.title.position = "center") +
  tm_fill('fa_sumest_polygon', title='# of registered individuals') + 
  tm_borders(col='grey',alpha=0.2)

hist(tett$fa_sumest_polygon, breaks=10000, xlim=c(0,10000))

tm_shape(tett, bbox = e) +
  tm_graticules() +
  tm_fill('ratio_km', title='Alien species - number of individuals per sqkm') + 
  tm_borders(col='grey',alpha=0.2)

tett %>%
  filter(ratio_m > 0.015)

tett[tett$ratio_m > 0.015,]

with(tett,plot(area,ratio_m, xlim=c(0,100000)))
