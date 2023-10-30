# read raw data
library(tidyverse)
library(here)
library(readxl)
library(sf)




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
  mutate(id = str_remove(id, "R"))|>
  relocate(site,taxon,id)|>
  group_by(site, taxon,id)|>
  summarize(across(-c(1:3), ~mean(.x, na.rm = TRUE)))|> # average the reruns
  ungroup()|>
  mutate(taxon = fct_collapse(taxon,
                              Brachycentridae = c("Brachycentridae", "Bracycentridae"),
                              Corydalidae = c("Corydalidae", "Corydalidae (small)", "Corydalidae (large)"),
                              Hirudinidae = "Hirundinidae"))|>
  group_by(site, taxon)|>
  summarize(across(-c(1:3), ~mean(.x, na.rm = TRUE)))|> #average the replicates
  mutate(taxon = as_factor(taxon))

#set ffg's
ffg_df <- read_csv(here("Raw_Data/taxa_list.csv"))

geos <- read_csv(here("Raw_Data/mapsInfo.csv"))|>
  st_as_sf(wkt="WKT")
  mutate(new_name = ifelse(!is.na(description), description, name))|>
  filter(!str_detect(new_name,"NEON"))|>
  mutate(new_name = str_remove(new_name, " Creek"),
         new_name = str_remove(new_name, " River"),
         new_name = fct_recode(new_name, "NF Ninnescah" = "N Fork Ninnescah"))|>
  rename(site = "new_name")
geo_names <- geos|>distinct(new_name)|>pull()

df <- fa_joined|>
  left_join(ffg_df)|>
  relocate(ffg)|>
  mutate(site = str_remove(site, " Creek"),
         site = str_remove(site, " River"),
         site = fct_recode(site, "N Canadian" = "N. Canadian", "Arkansas" = "Arkansas R."))|>
  left_join(geos|>select(WKT, site))|>
  relocate(site, WKT)


write_csv(df, here("Transformed_Data/tidied_df.csv"))

