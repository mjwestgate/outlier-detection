
# Crinia sloani example
c_sloani_data <- final_df |>
  filter(grepl("Crinia sloanei", scientificName)) |>
  select(decimalLatitude, decimalLongitude) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs(4326))

c_sloani_map <- species_df |>
  filter(species_name == "Crinia sloanei") |>
  pull(file) |>
  read_sf() |>
  select(geometry) |>
  st_transform(4326)

ggplot() + 
  geom_sf(data = c_sloani_map) +
  geom_sf(data = c_sloani_data)



# green tree frogs
final_df |>
  filter(grepl("Litoria phyllochroa", scientificName)) 
