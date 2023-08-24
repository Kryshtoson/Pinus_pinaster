library(tidyverse)
library(readxl)

read_xlsx('orig_data//Pinus_pinaster_Peninsular_Italy_20230201_layers_fused _tree_info.xlsx', 'spe') |>
  mutate(layer = ifelse(is.na(layer), 'T0', layer),
    species = paste(species, layer, sep = '_')) |>
  select(-layer) |>
  pivot_longer(-1, names_to = 'PlotID') |>
  pivot_wider(names_from = species) |>
  write_csv('meta//spe_new.csv')

read_xlsx('orig_data//Pinus_pinaster_Peninsular_Italy_20230201_layers_fused _tree_info.xlsx', 'head') |>
  select('PlotID' = 1, Longitude, Latitude, Dataset) |>
  write_csv('meta//head_new.csv')