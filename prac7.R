install.packages("stars","sf","tmaptools")
library("tmap")
library("stars")
library(here)
library(janitor)
library(sf)
library(tidyverse)
LondonWards <- st_read(here::here("London-wards-2018","London-wards-2018_ESRI", "London_Ward.shp"))
LondonWardsMerged <- st_read(here::here("London-wards-2018",
                                        "statistical-gis-boundaries-london",
                                        "ESRI",
                                        "London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700) #Returns a new geometry with its coordinates transformed to a different spatial reference system
WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                     locale = locale(encoding = "latin1"),
                     na = c("NA", "n/a")) %>% 
clean_names()

LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)
st_crs(LondonWardsMerged)
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")
library(tmap)

BluePlaques <- st_read(here::here("London-wards-2018","open-plaques-london-2018-04-08.geojson")) %>%
  st_transform(.,27700)
tmap_mode("plot")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

BluePlaquesSub <- BluePlaques[LondonWardsMerged,]
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")
example<-st_intersects(LondonWardsMerged, BluePlaquesSub) # we don’t want to use st_join() (which by default uses st_intersects()) as st_join() is like a left join and will retain all the data in on the left. So if a ward is empty, there will still be a row for that ward. So, we use st_intersects() on its own. We could also use other topological relationship functions such as st_within() instead

example
check_example <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%
  filter(ward_name=="Kingston upon Thames - Coombe Hill")
library(sf)
points_sf_joined <- LondonWardsMerged%>%
  mutate(n = lengths(st_intersects(., BluePlaquesSub)))%>% #we just take the length of each list per polygon and add this as new column
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)
library(spdep)
coordsW <- points_sf_joined%>%
  st_centroid()%>% # calculate the centroids of all rows
  st_geometry()

#simple binary matrix of queen’s case neighbours (otherwise known as Contiguity edges corners). This method means that polygons with a shared edge or a corner will be included in computations for the target polygon
plot(coordsW,axes=TRUE)
summary(LWard_nb)
plot(LWard_nb, st_geometry(coordsW), col="red")
LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T) #find the nieghbors and use the queens rule (forwards,backwards,on a diagnol)
summary(LWard_nb)
#add a map underneath
plot(points_sf_joined$geometry, add=T)
Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")
#^plotting the neighbors

sum(Lward.lw)
sum(Lward.lw[1,])
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")
#Moran’s I test tells us whether we have clustered values (close to 1) or dispersed values (close to -1)
I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw) #Morans I requires a spatial list instead of matrix
# the different ways to standardize rows: B is the basic binary coding (1/0)
#W is row standardised (sums over all links to n)
#C is globally standardised (sums over all links to n)
#U is equal to C divided by the number of neighbours (sums over all links to unity)
#S is the variance-stabilizing coding scheme proposed by Tiefelsdorf et al. 1999, p. 167-168 (sums over all links to n).

I_LWard_Global_Density
C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw) #Geary's C This tells us whether similar values or dissimilar values are clustering

C_LWard_Global_Density
G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw) #This tells us whether high or low values are clustering. If G > Expected = High values clustering; if G < expected = low values clustering

G_LWard_Global_Density
I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)
points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
library(RColorBrewer)
MoranColours<- rev(brewer.pal(8, "RdGy"))
tm_shape(points_sf_joined) +
tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=rgb,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)
points_sf_joined <- points_sf_joined %>%
mutate(density_G = as.numeric(Gi_LWard_Local_Density))
library(RColorBrewer)

GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")
# local Moran’s I and  G∗istatistics for wards clearly show that the density of blue plaques in the centre of the city exhibits strong (and positive) spatial autocorrelation
slice_head(points_sf_joined, n=2)
Datatypelist <- LondonWardsMerged %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist
I_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocIz = as.numeric(I_LWard_Local_GCSE$Z.Ii))


tm_shape(points_sf_joined) +
  tm_borders("Blue", lwd = 2) +
  tm_fill("ward_name")
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")         
  G_LWard_Local_GCSE <- LondonWardsMerged %>%
    dplyr::arrange(GSS_CODE)%>%
    dplyr::pull(average_gcse_capped_point_scores_2014) %>%
    as.vector()%>%
    localG(., Lward.lw)
  
  points_sf_joined <- points_sf_joined %>%
    dplyr::arrange(gss_code)%>%
    dplyr::mutate(GCSE_LocGiz = as.numeric(G_LWard_Local_GCSE))
  
  tm_shape(points_sf_joined) +
    tm_polygons("GCSE_LocGiz",
                style="fixed",
                breaks=breaks1,
                palette=GIColours,
                midpoint=NA,
                title="Gi*, GCSE Scores")
  