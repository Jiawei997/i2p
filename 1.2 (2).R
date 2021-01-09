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
library(leafpop)
library(leaflet)
#install.packages ("multcomp" )
library (multcomp)
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
  st_centroid()#%>%
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

#=====================social-economic data processing===========================

#racial 

block_ethnic[is.na(block_ethnic)] <- 0

racial <- block_ethnic %>%
  mutate(black_percentage=block_ethnic$B03002004/block_ethnic$B03002001*100)%>%
  mutate(white_percentage=block_ethnic$B03002003/block_ethnic$B03002001*100)%>%
  mutate(hispanic_percentage=block_ethnic$B03002012/block_ethnic$B03002001*100)%>%
  mutate(population=block_ethnic$B03002001)

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
race_th=70

socio_economic<-racial[,c(1,24:ncol(racial))]%>%
  st_drop_geometry()

socio_economic$racial_group=case_when(
  socio_economic$black_percentage > race_th ~1,#Black
  socio_economic$white_percentage > race_th ~2,#White
  socio_economic$hispanic_percentage > race_th ~3,#Hispanic
  socio_economic$hispanic_percentage < race_th & socio_economic$white_percentage < race_th &
    socio_economic$black_percentage < race_th ~ 4#Racial mixed
)

#education

#class(education[,c(1,38:40)])

socio_economic<-left_join(socio_economic,education[,c(1,38:ncol(education))],by = c("geoid" = "geoid"))

socio_economic$educational_attainment=case_when(
  socio_economic$No_degree >= socio_economic$Highschool_gra & 
    socio_economic$No_degree> socio_economic$College_and_higher_degree ~1,#"No_degree"
  
  socio_economic$Highschool_gra > socio_economic$No_degree & 
    socio_economic$Highschool_gra >= socio_economic$College_and_higher_degree ~2,#"Highschool_degree"
  
  socio_economic$College_and_higher_degree > socio_economic$Highschool_gra & 
    socio_economic$College_and_higher_degree > socio_economic$No_degree ~3,#"College_and_higher_degree"
)
#income
#class(socio_economic)

socio_economic<-left_join(socio_economic,income[,c(1,20:ncol(income))],by = c("geoid" = "geoid"))

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

socio_economic<-left_join(socio_economic,accessibility_block[,c(1,23:25)],by = c("geoid" = "geoid"))

class(socio_economic)
socio_economic[is.na(socio_economic)] <- 0

#-----------------------------plot
socio_economic<-sf::st_as_sf(socio_economic, crs = 3078)

socio_economic <- sf::st_transform(socio_economic,"+proj=longlat +datum=WGS84")

tmap_mode("plot")

tm1 <- tm_shape(socio_economic) + 
  tm_polygons("distance", 
              breaks=c(0,800,1500,3500,7000),
              palette="Blues",
              labels = c('0-800','800-200','2000-4000','4000-7000'))+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.3)

tm1

tm2 <- tm_shape(socio_economic) + 
  tm_polygons("racial_group", 
              breaks=c(0,1.5,2.5,3.5,4.5),
              palette="Blues",
              labels = c('Black','White','Hispanic','Racial Mixed'))+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)+
  tm_credits("(b)", position=c(0,0.85), size=1.3)

tm2
unique(socio_economic$educational_attainment)

tm3 <- tm_shape(socio_economic) + 
  tm_polygons("household_income", 
              breaks=c(0,1.5,2.5,3.5,4.5),
              palette="Blues",
              labels = c('<30k','30k-50k','50k-100k','>=100k'))+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)+
  tm_credits("(c)", position=c(0,0.85), size=1.3)

tm3

#breaks=c(0,1.5,2.5,3.5)
tm4 <- tm_shape(socio_economic) + 
  tm_polygons("educational_attainment", 
              breaks=c(0,1.5,2.5,3.5),
              palette="Blues",
              labels = c('No degree','High school','College'))+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)+
  tm_credits("(d)", position=c(0,0.85), size=1.3)
tm4

legend <- tm_shape(socio_economic) + 
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)
  #tm_credits("(c) OpenStreetMap contrbutors", position=c(0.0,0.0))
legend <- tm_shape(socio_economic) + 
  tm_polygons("distance",
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)
  #tm_credits("(c) OpenStreetMap contrbutors", position=c(0.0,0.0))
t=tmap_arrange(tm1,tm2,tm3,tm4, ncol=2)

t
#--------------------------hot spot analysis

socio_economic<-sf::st_as_sf(socio_economic)%>%
  st_transform(3078)

socio_economic1_sp <- sf:::as_Spatial(socio_economic$geometry)


#First calculate the centroids of all Wards in London

coordsW <- socio_economic%>%
  st_centroid()%>% #gives the centroid of a geometry
  st_geometry()    #get rid of it we get the plot of all attributes,otherwise we get the location point

plot(coordsW,axes=TRUE)# axes:add y x axis or not


#create a neighbours list

LWard_nb <- socio_economic %>%
  poly2nb(., queen=T)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(socio_economic$geometry, add=T)#add to the base map

#create a spatial weights object from these weights
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)

#hot spot
Gi_LWard_distance <- socio_economic %>%
  pull(distance) %>%
  as.vector()%>%
  localG(., Lward.lw)

socio_economic <- socio_economic %>%
  mutate(distance_G = as.numeric(Gi_LWard_distance))

GIColours<- rev(brewer.pal(8, "RdBu"))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
#now plot on an interactive map
tm_shape(socio_economic) +
  tm_polygons("distance_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Accessibility in Metropolitan Detroit",
              labels = c('High Access, p=0.001','High Access, p=0.01','High Access, p=0.05','No Significance',
                         'Low Access, p=0.05','Low Access, p=0.01','Low Access, p=0.001'))+
  tm_layout(frame = FALSE,
            legend.outside.size = 0.1,
            legend.position = c(0.6, -0.05))+
  tm_scale_bar(position=c(0.01,-0.02), text.size=0.7)+
  tm_compass(north=0, position=c(0.1,0.1))

#-------------------distance classification

socio_economic$access=case_when(
  socio_economic$distance_G > 1.65 ~"Low_Access",#"High Access"
  socio_economic$distance_G < -1.65 ~"High_Access",#"Low Access"
  socio_economic$distance_G < 1.65 & socio_economic$distance_G > -1.65 ~"No Significance"#"No Significance"
)

tm_shape(socio_economic) +
  tm_polygons("distance_G",
              style="fixed",
              breaks=c(-1000,-1.65,1.65,1000),
              palette=GIColours,
              midpoint=NA,
              title="Accessibility clusters",
              labels = c('High Access','No Significance','Low Access'))+
  tm_layout(frame = FALSE,
            legend.outside.size = 0.3,
            legend.position = c(0.6, 0.01))+
  tm_scale_bar(position=c(0.01,-0.02), text.size=0.7)+
  tm_compass(north=0, position=c(0.1,0.1))

#------------------------------interactive map

#break_distance=c(0,1000,2000,3500,6000)
#breaks=c(0,1.5,2.5,3.5,4.5)

socio_economic<-sf::st_as_sf(socio_economic, crs = 3078)

socio_economic <- sf::st_transform(socio_economic,"+proj=longlat +datum=WGS84")

class(socio_economic)
#st_crs(socio_economic)

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

popupdistance_G <-socio_economic %>%
  st_drop_geometry()%>%
  dplyr::select(distance_G, geoid)%>%
  popupTable()

#breaks=c(0,1.5,2.5,3.5,4.5)
tmap_mode("view")

pal1 <- socio_economic %>%
  colorBin(palette = c("#FFC125", "#8A4117", "#7D0552", "#571B7E"), domain=.$racial_group,bin=c(0,1.5,2.5,3.5,4.5))

pal2 <- socio_economic %>%
  colorBin(palette = c("#FFC125", "#8A4117", "#7D0552"), domain=.$educational_attainment,bin=c(0,1.5,2.5,3.5))

pal3 <- socio_economic %>%
  colorBin(palette = c("#FFC125", "#8A4117", "#7D0552", "#571B7E"), domain=.$household_income,bin=c(0,1.5,2.5,3.5,4.5))

pal4 <- socio_economic %>%
  colorBin(palette = c("#FFC125", "#8A4117", "#7D0552"), domain=.$distance_G, bin=c(-1000,-1.65,1.65,1000))


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
              group = "racial_group"
              )%>%
  
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
  addPolygons(fillColor = ~pal4(distance_G), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              popup = popupdistance_G,
              fillOpacity = 0.7,group = "distance_G")%>%
  # add a legend
  addLegend(values = ~ socio_economic$racial_group,# labels=palette() ,
            #layerId= c(socio_economic$racial_group,socio_economic$household_income,socio_economic$educational_attainment),
            #group = c("racial_group","educational_attainment","household_income","distance"), 
            position ="bottomleft", title = "racial group",
            colors=c("#FFC125", "#8A4117", "#7D0552", "#571B7E"),
            labels = c("Black","White","Hispanic","Racial Mixed")) %>%
  addLegend(values = ~ socio_economic$educational_attainment,
            #layerId= socio_economic$educational_attainment,
            #group = "educational_attainment", 
            position ="bottomleft", title = "education",
            colors=c("#FFC125", "#8A4117", "#7D0552"),
            labels = c('No degree','High school','College')) %>% 
  addLegend(values = ~ socio_economic$household_income,
            #layerId= socio_economic$educational_attainment,
            #group = "educational_attainment", 
            position ="bottomleft", title = "income",
            colors=c("#FFC125", "#8A4117", "#7D0552", "#571B7E"),
            labels = c('<30k','30k-50k','50k-100k','>=100k')) %>% 
  addLegend(values = ~ socio_economic$distance_G,
            #layerId= socio_economic$educational_attainment,
            #group = "educational_attainment", 
            position ="bottomleft", title = "accessibility",
            colors=c("#FFC125", "#8A4117", "#7D0552"),
            labels = c('High Access','No Significance','Low Access')) %>% 
  # specify layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite", "CartoDB"),
    overlayGroups = c("racial_group", "educational_attainment","household_income","distance_G"),
    options = layersControlOptions(collapsed = FALSE)
  )

map


#--------------------socio_economic_sum---------------------statistics

socio_economic_sum<-as.data.frame(socio_economic[,c(1,ncol(socio_economic))])

racial_sum <- left_join(racial,socio_economic_sum,by = c("geoid" = "geoid"))%>%
  st_drop_geometry()

education_sum <- left_join(education, socio_economic_sum, by = c("geoid" = "geoid"))%>%
  st_drop_geometry()

income_sum <- left_join(income, socio_economic_sum, by = c("geoid" = "geoid"))%>%
  st_drop_geometry()

racial_sum <-racial_sum%>%
  mutate(Black=.$B03002004,
         White=.$B03002003,
         Hispanic=.$B03002012)

education_sum <-education_sum%>%
  mutate(No_Degree=.$No_degree*.$B15002001/100,
         Highschool_Degree=.$Highschool_gra*.$B15002001/100,
         College_Degree_and_above=.$College_and_higher_degree*.$B15002001/100)


income_sum$Less_than_30k_n <-block_wealth$B19001002+block_wealth$B19001003+
                          block_wealth$B19001004+block_wealth$B19001005+block_wealth$B19001006

income_sum$Within_30k_50k_n <-block_wealth$B19001007+block_wealth$B19001008+
                           block_wealth$B19001009+block_wealth$B19001010

income_sum$Within_50k_100k_n <-block_wealth$B19001011+block_wealth$B19001012+
                            block_wealth$B19001013

income_sum$Over_100k_n <-block_wealth$B19001014+block_wealth$B19001015+
                      block_wealth$B19001016+block_wealth$B19001017

income_sum[is.na(income_sum)] <- 0
#sum
summary_statistic <- left_join(racial_sum,income_sum,by = c("geoid" = "geoid"))

summary_statistic <-left_join(summary_statistic,education_sum,by = c("geoid" = "geoid"))

#cleaning
to_drop_1 <- grep("^B0", colnames(summary_statistic))

#summary_statistic <-subset(summary_statistic, select = -to_drop_1)

to_drop_2 <- grep("^B1", colnames(summary_statistic))

#summary_statistic <-subset(summary_statistic, select = -to_drop_2)

to_drop_3 <- grep("x$", colnames(summary_statistic))

#summary_statistic <-subset(summary_statistic, select = -to_drop_3)

to_drop_4 <- grep("y$", colnames(summary_statistic))

summary_statistic <-subset(summary_statistic, select = -c(to_drop_1,to_drop_2,to_drop_3,to_drop_4))
#------------------------(sum)------------------------------------
summary_statistic1 <-as.data.frame(summary_statistic)

unique(summary_statistic1$access)

dimnames(summary_statistic1)

summary_statistic_H<-summary_statistic1%>%
  filter(access=="High_Access")%>%
  #group_by(.,access)%>%
  group_by(.,access,add=TRUE)%>%
  summarise(access = unique(access),Black=sum(Black),White=sum(White),Hispanic=sum(Hispanic),
            Less_than_30k_n=sum(Less_than_30k_n),Within_30k_50k_n=sum(Within_30k_50k_n),Within_50k_100k_n=sum(Within_50k_100k_n),Over_100k_n=sum(Over_100k_n),
            No_Degree=sum(No_Degree),Highschool_Degree=sum(Highschool_Degree),College_Degree_and_above=sum(College_Degree_and_above))


summary_statistic_L<-summary_statistic1%>%
  filter(access=="Low_Access")%>%
  #group_by(.,access)%>%
  group_by(.,access,add=TRUE)%>%
  summarise(access = unique(access),Black=sum(Black),White=sum(White),Hispanic=sum(Hispanic),
            Less_than_30k_n=sum(Less_than_30k_n),Within_30k_50k_n=sum(Within_30k_50k_n),Within_50k_100k_n=sum(Within_50k_100k_n),Over_100k_n=sum(Over_100k_n),
            No_Degree=sum(No_Degree),Highschool_Degree=sum(Highschool_Degree),College_Degree_and_above=sum(College_Degree_and_above))


summary_statistic_C<-summary_statistic1%>%
  #filter(access=="Low_Access")%>%
  #group_by(.,access)%>%
  #group_by(.,access,add=TRUE)%>%
  summarise(Black=sum(Black),White=sum(White),Hispanic=sum(Hispanic),
            Less_than_30k_n=sum(Less_than_30k_n),Within_30k_50k_n=sum(Within_30k_50k_n),Within_50k_100k_n=sum(Within_50k_100k_n),Over_100k_n=sum(Over_100k_n),
            No_Degree=sum(No_Degree),Highschool_Degree=sum(Highschool_Degree),College_Degree_and_above=sum(College_Degree_and_above))%>%
  mutate(access="Total category")

summary_statistic_A<-summary_statistic1%>%
  #filter(access=="Low_Access")%>%
  #group_by(.,access)%>%
  #group_by(.,access,add=TRUE)%>%
  summarise(Black=sum(block_ethnic$B03002001),White=sum(block_ethnic$B03002001),Hispanic=sum(block_ethnic$B03002001),
            Less_than_30k_n=sum(block_wealth$B19001001),Within_30k_50k_n=sum(block_wealth$B19001001),Within_50k_100k_n=sum(block_wealth$B19001001),Over_100k_n=sum(block_wealth$B19001001),
            No_Degree=sum(block_edu$B15002001),Highschool_Degree=sum(block_edu$B15002001),College_Degree_and_above=sum(block_edu$B15002001))%>%
  mutate(access="Total")

count(summary_statistic1$access)

summary_statistic_all <- rbind(summary_statistic_H, summary_statistic_L)
#summary_statistic_all <- rbind(summary_statistic_all, summary_statistic_C)
summary_statistic_all <- rbind(summary_statistic_all, summary_statistic_A)

table <- data.frame(t(summary_statistic_all[,c(2:ncol(summary_statistic_all))]))

colnames(table) = summary_statistic_all$access

table$catagory=rownames(table)

table<-table%>%
  mutate(proportion_H=.$High_Access/.$Total*100,
         proportion_L=.$Low_Access/.$Total*100)
#---------------------------barplot-------------------------------------

ggplot(summary_statistic_all[c(1,2),], aes(factor(access), as.factor(Black,White))) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

library(tidyverse)
library(hrbrthemes)
library(babynames)
library(viridis)

bar <- as.data.frame(socio_economic[,c(5,6,ncol(socio_economic))])

bar<-bar  %>% 
  #filter(racial_group %in% c(1,2,3,4)) %>%
  filter(access %in% c("High_Access", "Low_Access")) %>%
  mutate(access=as.factor(access)) %>%
  ggplot(aes(x=access, y=population, fill=racial_group)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_viridis(discrete=TRUE, name="") +
  theme_ipsum() +
  ylab("Number of baby")

plot(bar)
#-----------------------ANOVA test----------------------------------------
#ANOVA<-socio_economic[,-ncol(socio_economic)]
ANOVA<-socio_economic

ANOVA$Race_ethnicity <- 0

ANOVA$Race_ethnicity=case_when(
  socio_economic$racial_group == 1 ~"Black",#Black
  socio_economic$racial_group == 2 ~"White",#White
  socio_economic$racial_group == 3 ~"Hispanic",#Hispanic
  socio_economic$racial_group == 4 ~"Racial_mixed" #Racial mixed
)

ANOVA$Education=case_when(
  socio_economic$educational_attainment ==1 ~"No_degree",#"No_degree"
  socio_economic$educational_attainment ==2 ~"High_school_degree",#"High_school_degree"
  socio_economic$educational_attainment ==3 ~"College_and_higher_degree",#"College_and_higher_degree"
)

ANOVA$Income=case_when(
  
  socio_economic$household_income ==1 ~ "Less_than_30k",#"Less_than_30k"
  socio_economic$household_income ==2 ~ "Within_30k_50k",#"Within_30k_50k"
  socio_economic$household_income ==3 ~ "Within_50k_100k",#"Within_50k_100k"
  socio_economic$household_income ==4 ~ "Over_100k",#"Over_100k"
  
)

#----------------------one factor
library(car)
leveneTest(distance~Race_ethnicity,data=ANOVA_1)

fit<-aov(distance~Race_ethnicity,data=ANOVA_1)
summary(fit)

#----------------------multiple factor

ANOVA_1 <- ANOVA[,c(22:24,16)]
summary(ANOVA_1)
#data(ANOVA_1)
dim(ANOVA_1)
boxplot(distance~Race_ethnicity,data=ANOVA_1,col=c(1:4),main="Grouped by Race-ethnicity")
boxplot(distance~Education,data=ANOVA_1,col=c(1:4))
boxplot(distance~Income,data=ANOVA_1,col=c(1:4))

boxplot(ANOVA_1$distance,data=ANOVA_1)

fit1<-aov(distance~Race_ethnicity+Education+Income,data=ANOVA_1)
summary(fit1)
#plot(fit1)

fit2<-aov(distance~Race_ethnicity*Education*Income,data=ANOVA_1)
summary(fit2)

#install.packages("HH")
library(HH)
interaction2wt(distance~Race_ethnicity*Education,data=ANOVA_1)





