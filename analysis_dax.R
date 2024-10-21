

library(galah)
library(tidyverse)
library(sf)
library(here)
library(ozmaps)
library(tidymodels)
library(tidysdm)


data <- readRDS(here("frog_outliers.rds"))


data_filtered <- data %>%
  select(where(~ n_distinct(.) > 1)) |>
  filter(distance_from_land == 0) |>
  mutate(
    response = ifelse(as.integer(distance_from_distribution > 1000), "yes", "no")
  ) |>
  drop_na() |>
  select(-taxonConceptID, -dataGeneralizations, -dataProviderUid, -datasetID, 
         -datasetName, -dataProviderName)



frogs_sf <- data_filtered |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) |>
  st_set_crs(4326)


custom_bbox <- tibble(ymin = -45, 
                      ymax = -10, 
                      xmin = 110, 
                      xmax = 155)

aus <- ozmaps::ozmap_states |>
  st_transform(crs = st_crs(4326))




# set training and testing data
set.seed(100)

frogs_split <- 
  data_filtered |>
  initial_split()

frogs_split

frogs_train <- training(frogs_split)
frogs_test <- testing(frogs_split)


# resampling
set.seed(100)
frogs_cv <- vfold_cv(frogs_train, v = 5)

# define model
frogs_recipe <- recipe(
  frogs_train, 
  formula = response ~ .
  ) %>%
  update_role(recordID, new_role = "ID") %>%
  step_dummy(all_nominal())

# frogs_recipe |>
#   prep() |>
#   juice()

# specify model
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# create workflow 
tune_wf <- workflow() %>%
  add_recipe(frogs_recipe) %>%
  add_model(tune_spec)


# tune model
doParallel::registerDoParallel()

tune_res <- tune_grid(
  tune_wf,
  resamples = frogs_cv,
  grid = 20
)

# show_notes(.Last.tune.result)



