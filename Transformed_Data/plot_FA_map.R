library(tidyverse)
library(here)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)

geos_df <- read_csv(here("Transformed_Data/tidied_df.csv"))|>
  mutate(WKT = str_remove(WKT, fixed("POINT (")),
         WKT = str_remove(WKT, fixed(")")))|>
  separate(WKT, into = c("longitude", "latitude"), sep = " ")

write_csv(geos_df|>select(longitude,latitude,site)|>distinct(),"latlongs.csv")

hp <- geos_df|>filter(taxon == "Hydropsychidae")|>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

gp_states <- tigris::states(cb = TRUE, resolution = "20m", class = "sf") %>%
  filter(STUSPS %in% c("TX", "OK", "KS", "NE", "SD", "ND"))

all_states<-tigris::states(cb = TRUE, resolution = "20m", class = "sf")



tm_basemap("OpenTopoMap")+
  tm_shape(all_states)
tm_shape(all_states, is.main = TRUE, bbox = tmaptools::bb(all_states, xlim = c(-107,-93), ylim = c(26,49)))+
  tm_polygons(fill = "white")+
  tm_shape(gp_states)+
  tm_polygons()


tm_shape(all_states, is.main = TRUE, bbox = tmaptools::bb(all_states, xlim = c(-107,-93), ylim = c(26,49)))+
  tm_polygons()+
tm_shape(hp)+
  tm_symbols(alpha = 0.2, size = "22:5w6")
