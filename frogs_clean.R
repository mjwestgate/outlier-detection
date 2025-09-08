library(tidyverse)

frogs <- readRDS('frog_outliers.rds')

frogs_clean <- frogs %>%
  filter(distance_from_land == 0) %>% 
  mutate(outside_range = ifelse(distance_from_distribution > 1000, TRUE, FALSE),
         coord_sum = rowSums(select(., 27:45)),
         taxo_sum = rowSums(select(., where(is.numeric) & contains('taxo', ignore.case = TRUE))),
         log_uncert = log10(coordinateUncertaintyInMeters + 1),
         mean_uncert = mean(log_uncert, na.rm = TRUE),
         log_uncert_fixed = tidyr::replace_na(log_uncert, mean_uncert[1]),
         is_specimen = ifelse(basisOfRecord %in% c('MATERIAL_SAMPLE', 'PRESERVED_SPECIMEN'), 1, 0)) %>%
  select(eventDate,
         is_citsci,
         is_specimen,
         outside_range,
         taxo_sum,
         coord_sum,
         log_uncert_fixed)

write.csv(frogs_clean,'frogs_clean.csv')


frogs |>
  filter(eventDate < ymd("1600-01-01")) |>
  group_by(dataResourceUid) |>
  summarize(count = n())

library(galah)
result <- galah_call() |>
  filter(dataResourceUid == "dr710",
         year == 1599) |>
  count() |>
  collapse()
result$url
