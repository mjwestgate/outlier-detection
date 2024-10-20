library(here)
library(dplyr)
library(sf)
# remotes::install_github("atlasoflivingaustralia/galah-R@dev")
library(galah)
library(stringr)
library(ggplot2)
library(glue)
library(purrr)

# get a list of all shapefiles
species_files <- list.files(
  path = here("FrogID_species_map_shapefiles"),
  pattern = ".shp$")

# parse to a tibble including species names
species_df <- tibble(
  species_name = {species_files |>
      str_replace(".shp$", "") |>
      str_replace("_", " ")
      },
  file = glue("{here()}/FrogID_species_map_shapefiles/{species_files}"))
rm(species_files)

# get list of all frogs in ALA, with record counts
galah_config(email = "martinjwestgate@gmail.com")
record_count <- galah_call(type = "species") |>
  filter(class == "Amphibia") |>
  select(group = "taxonomy", counts) |>
  collect()

# join the above two datasets
species_df <- left_join(
  species_df,
  record_count,
  by = "species_name") |> 
  filter(count > 0)
rm(record_count)

## how many records are there?
# sum(species_df$count) # 1.56 M
## make the assumption that all frogID records are within their own polygons
## how many records now?
# search_all(datasets, "frogID") # dr14760
# galah_call() |>
#   filter(class == "Amphibia", 
#          dataResourceUid != "dr14760") |>
#   count() |>
#   collect()
## 850k are _not_ FrogID

# get all data for this taxon in a single download
ala_frog_data <- galah_call() |>
  filter(class == "Amphibia",
         dataResourceUid != "dr14760",
         occurrenceStatus == "PRESENT") |> 
  select( # spatial
    coordinateUncertaintyInMeters,
    coordinatePrecision,
    datePrecision,
    spatiallyValid,
    geospatialIssues,
    geospatialIssuesCount,
    dataGeneralizations,
    # collectory
    datasetID,
    datasetName,
    dataResourceUid,
    dataResourceName,
    dataProviderUid,
    dataProviderName,
    group = c("basic", "assertions")) |>
  collect(mint_doi = TRUE)

# example code for downloading from DOI:
# https://doi.org/10.26197/ala.bf084dea-b1ed-4a06-87ac-efcc4e015539
ala_frog_data <- galah_call() |>
  filter(doi == "https://doi.org/10.26197/ala.bf084dea-b1ed-4a06-87ac-efcc4e015539") |>
  collect()
# nrow(ala_frog_data) # 850k


# pull in maps and compare for outliers
sf_use_s2(FALSE) # necessary to prevent bugs for overlapping points

final_df <- map(
  # arrange in decreasing order of size
  {species_df |>
      arrange(desc(count)) |>
      split(f = seq_len(nrow(species_df)))},
  \(a){
    data_df <- ala_frog_data |>
      filter(taxonConceptID == a$taxon_concept_id,
             !is.na(decimalLatitude),
             !is.na(decimalLongitude))
    if(nrow(data_df) < 1){
      NULL
    }else{
      points_df <- data_df |>
        select(decimalLatitude, decimalLongitude) |>
        st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
                 crs = st_crs(4326))
      shape_df <- read_sf(a$file) |>
        select(geometry) |>
        st_transform(4326)
      
      # OK fastest way to do this is get within first
      data_df$distance_from_distribution <- 0
      within <- st_intersects(x = points_df, 
                              y = shape_df) |>
        lengths() # each list entry has IDs of the polygon it is within
        # ergo lengths == 0 is no polygons
      # then calculate only distances where within == NA
      outside_points <- within < 1
      if(any(outside_points)){
        rows <- which(outside_points)
        distance <- st_distance(x = points_df[rows, ], 
                                y = shape_df,
                                tolerance = 50)
        data_df$distance_from_distribution[rows] <- apply(distance, 
                                                          1, 
                                                          function(b){min(b[which(b > 0)])})
      }
      data_df
      # ## example plot
      # ggplot() +
      #   geom_sf(data = shape_df,
      #           fill = "red") +
      #   geom_sf(data = points_df,
      #           # aes(color = distance),
      #           shape = 1,
      #           size = 1)
    }
  },
  .progress = TRUE) |>
  bind_rows()
saveRDS(final_df, "frog_outliers.rds")



# calculate distance from land
# this is on the basis that filtering out oceanic records should be straightforward
aus <- read_sf(here("AUS_2021_AUST_SHP_GDA2020", "AUS_2021_AUST_GDA2020.shp")) |>
  slice_head(n = 1) |>
  select(geometry) |>
  st_transform(4326)

# ggplot(aus) + geom_sf()
final_df$distance_from_land <- 0
points_df <- final_df |>
  select(decimalLatitude, decimalLongitude) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs(4326))
within <- st_intersects(x = points_df, 
                        y = aus) |>
  lengths() # each list entry has IDs of the polygon it is within
rows <- which(within < 1)
distance <- st_distance(x = points_df[rows, ], 
                        y = aus,
                        tolerance = 50)
final_df$distance_from_land[rows] <- apply(distance, 
                                           1, 
                                           function(b){min(b[which(b > 0)])})
saveRDS(final_df, "frog_outliers.rds")