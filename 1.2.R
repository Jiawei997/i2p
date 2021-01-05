library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(mapview)
library(tidyverse)
library(fs)
library(geosphere)
library(dplyr)
#-------------------ethnic block
block_ethnic <- st_read(here::here("data", "us-census", "ethnic","detroit-census-block",
                                     "acs2019_5yr_B03002_15000US261635154002.shp"))

to_drop <- grep("e$", colnames(block_ethnic))

block_ethnic <-subset(block_ethnic, select = -to_drop)


#------------------income block
block_wealth <- st_read(here::here("data", "us-census", "wealth","detroit-census-block",
                                   "acs2019_5yr_B19001_15000US261635154002.shp")) %>%  
  st_transform(., 3078)

to_drop <- grep("e$", colnames(block_wealth))

block_wealth <-subset(block_wealth, select = -to_drop)

#------------------education block
block_edu <- st_read(here::here("data", "us-census", "education","acs2019_5yr_B15002_15000US261635154002.shp")) %>%  
  st_transform(., 3078)

to_drop <- grep("e$", colnames(block_edu))

block_edu <-subset(block_edu, select = -to_drop)

#------------------OSM POI
poi_m <- st_read(here::here("data",                      
                          "michigan-latest-free.shp",  
                          "gis_osm_pois_free_1.shp")) 
#-----------------clip POI
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


#-----------------------select HEALTHY
unique(detroit_poi$fclass)

health_poi<-detroit_poi%>%
  filter(detroit_poi$fclass=="supermarket"|detroit_poi$fclass=="greengrocer"|detroit_poi$fclass=="restaurant"
         )
summary(health_poi)

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

plot(st_geometry(centroid_block), col="black")



#-----------------------------distance

library(geosphere)

A <- data.frame(Lat=centroid_block$Y, Long=centroid_block$X, Name=centroid_block$geoid)
B <- data.frame(Lat=health_poi$Y, Long=health_poi$X, Name=health_poi$osm_id)

emptylist <- list()

for (i in 1:nrow(A)) {
  emptylist <- list()
    for (j in 1:nrow(B)) {
      emptylist[[j]] <- distVincentyEllipsoid(A[i,c('Long','Lat')], B[j,c('Long','Lat')])
    }
  A$distance[i]<-min(as.numeric(emptylist))
}

#access<-sf::st_as_sf(A, coords = c("Long", "Lat"), crs = 3078)
#class(access)

access<-as.data.frame(A)

block_ethnic%>%
  st_transform(., 3078)
class(block_ethnic)

st_crs(block_ethnic)


accessibility_block<-left_join(block_ethnic,access,by = c("geoid" = "Name"))

#---------------------visualization
library(RColorBrewer)
display.brewer.all()
breaks = c(0, 500, 1000, 2000, 4000, 5000)

tmap_mode("plot")

tm1 <- tm_shape(accessibility_block) + 
  tm_polygons("distance", 
              breaks=breaks,
              palette="Blues")+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

tm1

#=====================social-economic data processing===========================

#racial 
racial <- block_ethnic %>%
  mutate(black_percentage=block_ethnic$B03002004/block_ethnic$B03002001*100)%>%
  mutate(white_percentage=block_ethnic$B03002003/block_ethnic$B03002001*100)%>%
  mutate(hispanic_percentage=block_ethnic$B03002012/block_ethnic$B03002001*100)

#income
income <-block_wealth

income$Less_than_30k <-(block_wealth$B19001002+block_wealth$B19001003+
                          block_wealth$B19001004+block_wealth$B19001005+block_wealth$B19001006)/block_wealth$B19001001*100

income$Within_30k_50k <-(block_wealth$B19001007+block_wealth$B19001008+
                          block_wealth$B19001009+block_wealth$B19001010)/block_wealth$B19001001*100

income$Within_50k_100k <-(block_wealth$B19001011+block_wealth$B19001012+
                           block_wealth$B19001013)/block_wealth$B19001001*100

income$Over_100k <-(block_wealth$B19001014+block_wealth$B19001015+
                           block_wealth$B19001016+block_wealth$B19001017)/block_wealth$B19001001*100

#education
education <-block_edu %>%
  mutate(No_degree=(.$B15002003+.$B15002004+.$B15002005+.$B15002006+.$B15002007+.$B15002008+.$B15002009+.$B15002010
         +.$B15002020+.$B15002020+.$B15002021+.$B15002022+.$B15002023+.$B15002024+.$B15002025+.$B15002026+.$B15002027)/.$B15002001*100)%>%
  
  mutate(Highschool_gra=(.$B15002011+.$B15002028)/.$B15002001*100)%>%
  
  mutate(College_and_higher_degree=(.$B15002015+.$B15002016+.$B15002017+.$B15002018+.$B15002032+.$B15002033+.$B15002034+.$B15002035)/.$B15002001*100)

#------------------------------classification 

#racial
race_th=75

socio_economic<-racial[,c(1,24:26)]

socio_economic$racial_group=case_when(
  socio_economic$black_percentage > race_th ~"Black",
  socio_economic$white_percentage > race_th ~"White",
  socio_economic$hispanic_percentage > race_th ~"Hispanic",
  socio_economic$hispanic_percentage < race_th & socio_economic$white_percentage < race_th &
    socio_economic$black_percentage < race_th ~ "Racial mixed"
)

#education
edu_th=50

socio_economic<-education[,c(1,38:40)]

socio_economic<-left_join(socio_economic,education[,c(1,38:40)],by = c("geoid" = "geoid"))

socio_economic$educational_attainment=case_when(
  socio_economic$No_degree > socio_economic$Highschool_gra & socio_economic$College_and_higher_degree ~"No_degree",
  socio_economic$white_percentage > race_th ~"White",
  socio_economic$hispanic_percentage > race_th ~"Hispanic",
  socio_economic$hispanic_percentage < race_th&socio_economic$white_percentage < race_th&
    socio_economic$black_percentage < race_th~ "Racial mixed"
)


