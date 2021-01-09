block_ethnic1 <- block_ethnic %>%
  mutate(black_percentage=block_ethnic$B03002004/block_ethnic$B03002001*100)%>%
  mutate(white_percentage=block_ethnic$B03002003/block_ethnic$B03002001*100)%>%
  mutate(hispanic_percentage=block_ethnic$B03002012/block_ethnic$B03002001*100)


tract_wealth <- st_read(here::here("data", "us-census", "wealth","detroit-census-tracts",
                                   "acs2019_5yr_B19001_14000US26163512300.shp")) %>%  
  st_transform(., 3078)

block_wealth <- st_read(here::here("data", "us-census", "wealth","detroit-census-block",
                                   "acs2019_5yr_B19001_15000US261635154002.shp")) %>%  
  st_transform(., 3078)

block_wealth1<-block_wealth %>%
  mutate(less_than_30k=block_wealth$B19001002+block_wealth$B19001003+block_wealth$B19001004
         +block_wealth$B19001005+block_wealth$B19001006)%>%
  mutate(within_30k_50k=block_wealth$B19001007+block_wealth$B19001008+block_wealth$B19001009
         +block_wealth$B190010010)%>%
  mutate(within_50k_100k=block_wealth$B190010011+block_wealth$B190010012+block_wealth$B190010013)%>%
  mutate(over_100k=block_wealth$B190010014+block_wealth$B190010015+block_wealth$B190010016)%>%
  
  
block_wealth1 <- block_wealth %>%
  filter(CNTRY_NAME=='United Kingdom'&
           Worldcities$CITY_NAME=='Birmingham'|
           Worldcities$CITY_NAME=='London'|
           Worldcities$CITY_NAME=='Edinburgh')


library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(tmaptools)

#-------------------tract
tract_ethnic <- st_read(here::here("data", "us-census", "ethnic","detroit-census-tracts",
                                   "acs2019_5yr_B03002_14000US26163512300.shp"))

tmap_mode("view")
tm_shape(us_tract_ethnic) +
  tm_polygons(col = "B03002001", alpha = 0.5)

st_crs(us_tract_ethnic)
#-------------------block
block_ethnic <- st_read(here::here("data", "us-census", "ethnic","detroit-census-block",
                                   "acs2019_5yr_B03002_15000US261635154002.shp"))

tmap_mode("view")
tm_shape(block_ethnic) +
  tm_polygons(col = "B03002001", alpha = 0.5)


#------------------OSM
poi_m <- st_read(here::here("data",                      
                            "michigan-latest-free.shp",  
                            "gis_osm_pois_free_1.shp")) 


road_m <- st_read(here::here("data",                      
                             "michigan-latest-free.shp",  
                             "gis_osm_roads_free_1.shp")) %>%  
  st_transform(., 3078)
#-----------------clip
#dissolve
detroit <-
  block_ethnic %>%
  mutate(region = "detroit") %>%
  group_by(region) %>%
  summarise()

plot(detroit)

#clip
detroit_poi <- 
  poi_m %>% 
  filter(st_contains(detroit, ., sparse = FALSE))


plot(st_geometry(detroit_poi), col="black")

detroit_road <- 
  road_m %>% 
  filter(st_contains(detroit, ., sparse = FALSE))

plot(st_geometry(detroit_road), col="black")
#select
unique(detroit_poi$fclass)

health_poi<-detroit_poi%>%
  filter(detroit_poi$fclass=="supermarket"|detroit_poi$fclass=="greengrocer"|detroit_poi$fclass=="restaurant"
  )
coordinates1 <-st_coordinates(health_poi)

coordinates1 <-as.data.frame(coordinates1)

health_poi <- health_poi %>%
  mutate(X=coordinates1$X)%>%
  mutate(Y=coordinates1$Y)

#-----------------centroid

centroid_block <- block_ethnic %>%
  st_centroid()
#st_geometry()

coordinates <-st_coordinates(centroid_block)

coordinates <-as.data.frame(coordinates)

class(health_poi$X)

centroid_block <- centroid_block %>%
  mutate(X=coordinates$X)%>%
  mutate(Y=coordinates$Y)

#-----------------clean the df
centroid_block <-subset(centroid_block, select = -c(B03002001e,B03002002e,B03002003e,B03002004e,B03002005e,B03002006e,B03002007e,B03002008e,B03002009e,B03002010e,B03002011e,B03002012e,B03002013e,B03002014e,B03002015e,B03002016e,B03002017e,B03002018e,B03002019e,B03002020e,B03002021e))



#centroid_block<-centroid_block%>%
#  st_join(.,tract_ethnic)

plot(st_geometry(centroid_block), col="black")

#--------------distance

getNearest <- function(shp){
  dist <- as.data.frame(st_distance(shp),"Euclidean")
  for (i in 1:ncol(dist)){
    rows <- seq(1:ncol(dist))
    rows <- rows[i != rows]
    shp[i, 'nearest'] <- which.min(dist[rows, i])
    shp[i, 'distance'] <- dist[which.min(dist[rows, i]), i]
  }
  return(shp)
}

pts2 <- getNearest(pts)


?st_distance

#matrix
distMatrix <- shortest.paths(g, v=V(g), to=V(g))

#-----------------------------distance2

centroid_block%>%mutate(type="residential")

health_poi%>%mutate(type="food")

#block_poi <- rbind(centroid_block,health_poi)


centroid_block$distance<-dataset.distance(centroid_block, health_poi, x.name = "X", y.name = "Y", fun = mean, method = "euclidean")




library(geosphere)
centroid_block$distance<distm(c(centroid_block$X, centroid_block$Y), c(health_poi$X, health_poi$Y), fun = distHaversine)

library(geosphere)

df <- data.frame(lon = c(centroid_block$X, health_poi$X), lat=c(centroid_block$Y,health_poi$Y))
distance <- distGeo(df[1, ], df[2, ])




library(geosphere)

A <- data.frame(Lat=centroid_block$Y, Long=centroid_block$X, Name=centroid_block$geoid)
B <- data.frame(Lat=health_poi$Y, Long=health_poi$X, Name=health_poi$osm_id)

A[B,distance:=distVincentyEllipsoid(A[,.(Long,Lat)], B[,.(Long,Lat)])]



A$distance <- distVincentyEllipsoid(A[,c('Long','Lat')], B[,c('Long','Lat')])

centroid_block$distance <- distVincentyEllipsoid(centroid_block[,c('X','Y')], health_poi[,c('X','Y')])
library(data.table)
#centroid_block[health_poi,distance:=distVincentyEllipsoid(centroid_block[,.(X,Y)], health_poi[,.(X,Y)])]


id<-c("1","2")
lat<-c(42.44359,42.43845)
lon<-c(-82.94878,-82.95350)
d<-data.frame(id,lat,lon)%>%
  mutate(distance)
d
#d2<-as.matrix(d[,-c(1:2)])
lat<-c(42.35273,42.33142)
lon<-c(-83.06213,-83.04892)
D<-data.frame(id,lat,lon)
D
distance1<-list()

distance1 <- distVincentyEllipsoid(d[,c('lon','lat')], D[,c('lon','lat')])
distance<-min(distance)
distance
for (i in 1:nrow(d)) {
  for (j in 1:nrow(D)) {
    distance1 <- distVincentyEllipsoid(d[,c('lon','lat')],D[j,c('lon','lat')])
    d$distance<-min(as.numeric(distance1))
  }
}
#d <- data.table(d)

# CALCULATE DISTANCE (dist) TO EACH ID (dist_to)

for (i in 1:nrow(d)) {
  print (d[i,]$id)
  for (j in 1:nrow(d)) {
    
    d1 <- d[id == d[i,]$id, dist=distm(c(d[i,]$lon, d[i,]$lat), c(d[j,]$lon, d[j,]$lat), fun = distHaversine)/1000, ]
    d1 <- d1[, dist_to= d[j,]$id,]
    if(exists('d2')){ d2<-rbindlist(list(d2,d1))} else {d2<-copy(d1)}
  }
}






s<-nrow(d)
km2<-lapply(1:s,function(i) d[which(spDistsN1(d2,d2[i,],longlat=TRUE)<=500),])
km2
for (j in 1:196) {
  distance <- distVincentyEllipsoid(A[1,c('Long','Lat')], B[j,c('Long','Lat')])
  A$distance[1]<-max(distance)
}


B[1,c('Long','Lat')]
A$distance[1]
for (i in A) {
  for (j in B) {
    #distance <- distVincentyEllipsoid(i[,c('Long','Lat')], j[,c('Long','Lat')])}
    
    dis<-distGeo(A[i, ], B[j, ])
    A$distance<-min(distance)
    #distance <- distGeo(df[1, ], df[2, ])
  }}



A$distance <- distVincentyEllipsoid(A[,c('Long','Lat')], B[,c('Long','Lat')])


#A[B,distance:=distVincentyEllipsoid(A[,.(Long,Lat)], B[,.(Long,Lat)])]



A$distance <- distVincentyEllipsoid(A[,c('Long','Lat')], B[,c('Long','Lat')])

centroid_block$distance <- distVincentyEllipsoid(centroid_block[,c('X','Y')], health_poi[,c('X','Y')])
library(data.table)
#centroid_block[health_poi,distance:=distVincentyEllipsoid(centroid_block[,.(X,Y)], health_poi[,.(X,Y)])]



#---------------------accessibility

accessibility <- left_join(block_ethnic1, A,by = c("geoid" = "Name"))

accessibility%>%
  mutate(id=block_ethnic1$geoid)

accessibility$id <- substr(accessibility$geoid, 0, 18)

#str(accessibility)
accessibility1 <-accessibility%>%                    
  group_by(id) %>% 
  summarise(tract_pop = sum(B03002001))
#aggregate(accessibility$B03002001, by=list(id=accessibility$id), FUN=sum)
#aggregate(B03002001 ~ id, accessibility, sum)

accessibility <-accessibility%>%
  mutate(score1=accessibility$B03002001*accessibility$distance)%>%
  mutate(access=score1/accessibility1$tract_pop)

accessibility<-st_join(accessibility,accessibility1)
accessibility <-subset(accessibility, select = -c(B03002001e,B03002002e,B03002003e,B03002004e,B03002005e,B03002006e,B03002007e,B03002008e,B03002009e,B03002010e,B03002011e,B03002012e,B03002013e,B03002014e,B03002015e,B03002016e,B03002017e,B03002018e,B03002019e,B03002020e,B03002021e))

#####

education<-block_edu[c(2:36)]

education<-education%>%
  mutate(No_degree_male=colSums(education[,3:10]))

rlang::last_error()

education$No_degree_male <- colSums(c[,3:10])
education$No_degree_female <- colSums(education[,20:27])
#33333333333333333333333333333
income <-block_wealth %>%
  mutate(Less_than_30k = (.$B19001002+.$B19001003+.$B19001004+.$B19001005+.$B19001006)/.$B19001001*100,
         Within_30k_50k =(.$B19001007+.$B19001008+.$B19001009+.$B190010010)/.$B19001001*100,
         Within_50k_100k=(.$B190010011+.$B190010012+.$B190010013)/.$B19001001*100,
         Over_100k=(.$B190010014+.$B190010015+.$B190010016+.$B19001017)/.$B19001001*100)




income <-block_wealth %>%
  mutate(Less_than_30k=(.$B19001002+.$B19001003+.$B19001004+.$B19001005+.$B19001006)/.$B19001001*100)
  mutate(Within_30k_50k=block_wealth$B19001008)%>%
  mutate(Within_50k_100k=block_wealth$B19001002)%>%
  mutate(Over_100k=block_wealth$B19001002)

income <-block_wealth %>%
  mutate(Less_than_30k = .$B19001002,
         Within_30k_50k = .$B19001002)
  
income <-income %>%
  mutate(Within_30k_50k=.$B19001007+.$B19001008+.$B19001009+.$B190010010)

income <-income %>%  
  mutate(Within_50k_100k=(.$B190010011+.$B190010012+.$B190010013)/.$B19001001*100)

income <-income %>%    
  mutate(Over_100k=(.$B190010014+.$B190010015+.$B190010016+.$B19001017)/.$B19001001*100)
############################33333333333333333
tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Life_expectancy4map) + 
  tm_polygons("UKdiff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))

####################################################
tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))

########################advance interactive map##############################

# library for pop up boxes
library(leafpop)
library(leaflet)

#join data
Joined <- Airbnb%>%
  st_join(., Hotels, join = st_equals)%>%
  dplyr::select(GSS_CODE.x, NAME.x, `Accomodation count.x`, `Accomodation count.y`)%>%
  dplyr::rename(`GSS code` =`GSS_CODE.x`,
                `Borough` = `NAME.x`,
                `Airbnb count` = `Accomodation count.x`,
                `Hotel count`= `Accomodation count.y`)%>%
  st_transform(., 4326)


#remove the geometry for our pop up boxes to avoid
popup_socio_economic <-socio_economic %>%
  st_drop_geometry()%>%
  dplyr::select(racial_group,geoid)%>%
  popupTable()

popuphotel <-Joined %>%
  st_drop_geometry()%>%
  dplyr::select(`Hotel count`, Borough)%>%
  popupTable()

tmap_mode("view")

# set the colour palettes using our previously defined breaks


pal1 <- Joined %>%
  colorBin(palette = "YlOrRd", domain=.$`Airbnb count`, bins=breaks)

pal1 <-colorBin(palette = "YlOrRd", domain=Joined$`Airbnb count`, bins=breaks)

pal2 <- Joined %>%
  colorBin(palette = "YlOrRd", domain=.$`Hotel count`, bins=breaks)


map<- leaflet(Joined) %>%
  # add basemap options
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB")%>%
  
  #add our polygons, linking to the tables we just made
  addPolygons(color="white", 
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popupairbnb,
              fillOpacity = 0.7,
              fillColor = ~pal2(`Airbnb count`),
              group = "Airbnb")%>%
  
  addPolygons(fillColor = ~pal2(`Hotel count`), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              popup = popupairbnb,
              fillOpacity = 0.7,group = "Hotels")%>%
  # add a legend
  addLegend(pal = pal2, values = ~`Hotel count`, group = c("Airbnb","Hotel"), 
            position ="bottomleft", title = "Accomodation count") %>%
  # specify layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite", "CartoDB"),
    overlayGroups = c("Airbnb", "Hotels"),
    options = layersControlOptions(collapsed = FALSE)
  )

# plot the map
map