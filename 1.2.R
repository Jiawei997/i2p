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
library(spdep)
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
breaks = c(0, 500, 1000, 2000, 5000, 7000)

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

block_ethnic[is.na(block_ethnic)] <- 0

racial <- block_ethnic %>%
  mutate(black_percentage=block_ethnic$B03002004/block_ethnic$B03002001*100)%>%
  mutate(white_percentage=block_ethnic$B03002003/block_ethnic$B03002001*100)%>%
  mutate(hispanic_percentage=block_ethnic$B03002012/block_ethnic$B03002001*100)

racial[is.na(racial)] <- 0

#income
block_wealth[is.na(block_wealth)] <- 0

income <-block_wealth

income$Less_than_30k <-(block_wealth$B19001002+block_wealth$B19001003+
                          block_wealth$B19001004+block_wealth$B19001005+block_wealth$B19001006)/block_wealth$B19001001*100

income$Within_30k_50k <-(block_wealth$B19001007+block_wealth$B19001008+
                          block_wealth$B19001009+block_wealth$B19001010)/block_wealth$B19001001*100

income$Within_50k_100k <-(block_wealth$B19001011+block_wealth$B19001012+
                           block_wealth$B19001013)/block_wealth$B19001001*100

income$Over_100k <-(block_wealth$B19001014+block_wealth$B19001015+
                           block_wealth$B19001016+block_wealth$B19001017)/block_wealth$B19001001*100

income[is.na(income)] <- 0
#education
block_edu[is.na(block_edu)] <- 0

education <-block_edu %>%
  mutate(No_degree=(.$B15002003+.$B15002004+.$B15002005+.$B15002006+.$B15002007+.$B15002008+.$B15002009+.$B15002010
         +.$B15002020+.$B15002020+.$B15002021+.$B15002022+.$B15002023+.$B15002024+.$B15002025+.$B15002026+.$B15002027)/.$B15002001*100)%>%
  
  mutate(Highschool_gra=(.$B15002011+.$B15002028)/.$B15002001*100)%>%
  
  mutate(College_and_higher_degree=(.$B15002015+.$B15002016+.$B15002017+.$B15002018+.$B15002032+.$B15002033+.$B15002034+.$B15002035)/.$B15002001*100)

education[is.na(education)] <- 0
#------------------------------classification 

#racial
race_th=75

socio_economic<-racial[,c(1,24:26)]%>%
  st_drop_geometry()

socio_economic$racial_group=case_when(
  socio_economic$black_percentage > race_th ~1,#Black
  socio_economic$white_percentage > race_th ~2,#White
  socio_economic$hispanic_percentage > race_th ~3,#Hispanic
  socio_economic$hispanic_percentage < race_th & socio_economic$white_percentage < race_th &
    socio_economic$black_percentage < race_th ~ 4#Racial mixed
)

#education
class(education[,c(1,38:40)])

socio_economic<-left_join(socio_economic,education[,c(1,38:40)],by = c("geoid" = "geoid"))

socio_economic$educational_attainment=case_when(
  socio_economic$No_degree >= socio_economic$Highschool_gra & 
    socio_economic$No_degree> socio_economic$College_and_higher_degree ~1,#"No_degree"
  
  socio_economic$Highschool_gra > socio_economic$No_degree & 
    socio_economic$Highschool_gra >= socio_economic$College_and_higher_degree ~2,#"Highschool_degree"
  
  socio_economic$College_and_higher_degree > socio_economic$Highschool_gra & 
    socio_economic$College_and_higher_degree > socio_economic$No_degree ~3,#"College_and_higher_degree"
)
#income
class(socio_economic)

socio_economic<-left_join(socio_economic,income[,c(1,20:23)],by = c("geoid" = "geoid"))

socio_economic$household_income=case_when(
  
  socio_economic$Less_than_30k >=socio_economic$Within_30k_50k & 
    socio_economic$Less_than_30k> socio_economic$Within_50k_100k &
    socio_economic$Less_than_30k> socio_economic$Over_100k ~ 1,#"Less_than_30k"
  
  socio_economic$Within_30k_50k > socio_economic$Less_than_30k & 
    socio_economic$Within_30k_50k>=socio_economic$Within_50k_100k &
    socio_economic$Within_30k_50k> socio_economic$Over_100k ~ 2,#"Within_30k_50k"
  
  socio_economic$Within_50k_100k > socio_economic$Within_30k_50k & 
    socio_economic$Within_50k_100k> socio_economic$Less_than_30k &
    socio_economic$Within_50k_100k>=socio_economic$Over_100k ~ 3,#"Within_50k_100k"
  
  socio_economic$Over_100k > socio_economic$Within_30k_50k & 
    socio_economic$Over_100k> socio_economic$Within_50k_100k &
    socio_economic$Over_100k> socio_economic$Less_than_30k ~ 4,#"Over_100k"
  
)
#accessibility

socio_economic<-left_join(socio_economic,accessibility_block[,c(1,24:26)],by = c("geoid" = "geoid"))

class(socio_economic)
socio_economic[is.na(socio_economic)] <- 0
#--------------------Visualization
break_distance=c(800,1500,2500,7000)
breaks=c(0,1.5,2.5,3.5,4.5)
socio_economic<-sf::st_as_sf(socio_economic, crs = 3078)

socio_economic <- sf::st_transform(socio_economic,"+proj=longlat +datum=WGS84")

class(socio_economic)
st_crs(socio_economic)

popupracial_group <-socio_economic %>%
  st_drop_geometry()%>%
  dplyr::select(racial_group, geoid)%>%
  popupTable()

popupeducation <-socio_economic %>%
  st_drop_geometry()%>%
  dplyr::select(educational_attainment, geoid)%>%
  popupTable()

popuphousehold_income <-socio_economic %>%
  st_drop_geometry()%>%
  dplyr::select(household_income, geoid)%>%
  popupTable()

popupdistance <-socio_economic %>%
  st_drop_geometry()%>%
  dplyr::select(distance, geoid)%>%
  popupTable()

tmap_mode("view")

pal1 <- socio_economic %>%
  colorBin(palette = "YlOrRd", domain=.$racial_group,bin=breaks)

pal2 <- socio_economic %>%
  colorBin(palette = "YlOrRd", domain=.$educational_attainment,bin=breaks)

pal3 <- socio_economic %>%
  colorBin(palette = "YlOrRd", domain=.$household_income,bin=breaks)

pal4 <- socio_economic %>%
  colorBin(palette = "YlOrRd", domain=.$distance, bin=break_distance)

map<- leaflet(socio_economic) %>%
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
              popup = popupracial_group,
              fillOpacity = 0.7,
              fillColor = ~pal1(racial_group),
              group = "racial_group")%>%
  
  addPolygons(fillColor = ~pal2(educational_attainment), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              popup = popupeducation,
              fillOpacity = 0.7,group = "educational_attainment")%>%
  addPolygons(fillColor = ~pal3(household_income), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              popup = popuphousehold_income,
              fillOpacity = 0.7,group = "household_income")%>%
  # add a legend
  addLegend(pal = pal1, values = ~ educational_attainment, group = c("racial_group","educational_attainment","household_income"), 
            position ="bottomleft", title = "healthy food accessibility") %>%
  # specify layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite", "CartoDB"),
    overlayGroups = c("racial_group", "educational_attainment","household_income"),
    options = layersControlOptions(collapsed = FALSE)
  )

# plot the map
map
#-----------------------------plot
tmap_mode("plot")

breaks=c(0,800,1500,3500,7000)
tm1 <- tm_shape(socio_economic) + 
  tm_polygons("distance", 
              breaks=breaks,
              palette="Blues")+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

tm1

breaks=c(0,1.5,2.5,3.5,4.5)
tm2 <- tm_shape(socio_economic) + 
  tm_polygons("racial_group", 
              breaks=breaks,
              palette="Blues")+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

tm2

breaks=c(0,1.5,2.5,3.5,4.5)
tm3 <- tm_shape(socio_economic) + 
  tm_polygons("household_income", 
              breaks=breaks,
              palette="Blues")+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

tm3

breaks=c(0,1.5,2.5,3.5)
tm4 <- tm_shape(socio_economic) + 
  tm_polygons("educational_attainment", 
              breaks=breaks,
              palette="Blues")+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)
tm4

legend <- tm_shape(socio_economic) + 
  tm_polygons("distance",
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)+
  tm_credits("(c) OpenStreetMap contrbutors and Air b n b", position=c(0.0,0.0))

t=tmap_arrange(tm1,tm2,tm3,tm4,  ncol=2)

t
#--------------------------hot spot analysis



socio_economic<-sf::st_as_sf(socio_economic)%>%
  st_transform(3078)

socio_economic1_sp <- sf:::as_Spatial(socio_economic$geometry)

W_cont_el<-poly2nb(socio_economic1_sp,queen=T)

W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)

#socio_economic1_sp<-st_transform(socio_economic1_sp, CRS("+init=EPSG:3078"))
proj4string(socio_economic1_sp) <- CRS("+init=EPSG:3078")

socio_economic1_sp <- spTransform(socio_economic1_sp, CRS("+init=EPSG:3078"))

lg1 <- localG(socio_economic1_sp$distance, listw=W_cont_el_mat, zero.policy=T)

socio_economic1_sp$lg1 <- lg1[]

lm.palette <- colorRampPalette(c("blue","white", "red"), space = "rgb")

spplot(socio_economic1_sp, zcol="lg1", col.regions=lm.palette(20), main="Getis-Ord Gi* (z scores)", pretty=T)




