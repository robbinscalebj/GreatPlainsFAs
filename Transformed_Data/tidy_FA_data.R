# read raw data
library(tidyverse)
library(here)
library(readxl)
library(sf)
library(tmap)
library(tmaptools)

geos <- read_csv(here("Raw_Data/mapsInfo.csv"))|>
  st_as_sf(wkt = "WKT")|>
  mutate(new_name = ifelse(!is.na(description), description, name))|>
  filter(!str_detect(new_name,"NEON")) #eventually get lat longs into file


# load information on sample IDs needed to identify from FA runs
fa_ids <- read_csv(here("Raw_Data/FA_sample_inventory.csv"))|>
  rename(site = "Site Name", taxon = "Taxon (Family) Name", wet_mass_mg = "Total blot mass (mg)", id = "FA Tube ID")|>
  mutate(id = ifelse(id == "n/a", NA_real_, id))|>
  filter(!is.na(id))

# FA data
fa_raw <- map(c(3,5,7,9,11), ~read_excel(here("Raw_Data/FA profiles.xlsx"), sheet = .x))|>
  map(~mutate(., across(everything(), as.character)))|>
  list_rbind()|>
  select(-site)

fa_df <- fa_raw|>
  select(where(~!all(is.na(.x))))|>
  mutate(across(-c(1:3), as.numeric))|>
  mutate(total_FA = rowSums(across(-c(1:3)), na.rm=TRUE))|>
  mutate(across(-c(1:3), ~./total_FA*100), .keep = "unused")|>
  select(-total_FA, -sample, -info)

fa_joined <- fa_df|>left_join(fa_ids|>select(site,taxon,id), by = "id")|>
  relocate(site,taxon)|>
  #average the re-runs - denoted by R, average duplicates (they are analytical and don't provide info on variation)
