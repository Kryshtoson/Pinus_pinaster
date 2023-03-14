#' cleans data and stores them into:
#' 
#' produces: 1 basic map and 2 ordination
# -------------------------------------------------------------------------
library(patchwork)
library(writexl)
library(ggrepel)
library(vegan)
library(tidyverse)
library(readxl)
library(twinspan)
library(sf)
library(rnaturalearth)
# hello hello

Italy <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(admin == 'Italy')

selected <- read_xlsx('orig_data\\spe.xlsx') %>% mutate_all(as.numeric) %>% 
  pivot_longer(-1) %>% 
  filter(grepl('_T1', name)) %>% 
  filter(value != 0) %>% 
  mutate(pinpir = name == 'Pinus pinaster_T1') %>% 
  select(PlotID, name = pinpir, value) %>% 
  group_by(PlotID, name) %>% 
  summarise(value = sum(value)) %>% 
  pivot_wider(values_fill = 0) %>% 
  filter(`TRUE` > `FALSE`) %>% 
  select(PlotID)

spe <- read_xlsx('orig_data\\spe.xlsx') %>%
  filter(PlotID %in% selected[[1]]) %>%
  mutate_all(as.numeric) %>%
  pivot_longer(-1) %>%
  mutate(name = gsub('_T0', '', name),
         name = gsub('_T1', '', name)) %>%
  rename(species = name) %>%
  left_join(read_xlsx('orig_data\\spp_merge.xlsx') %>%
              mutate(species_new = ifelse(is.na(species_new),
                                          species,
                                          species_new))) %>%
  filter(species_new != 'NA') %>%
  group_by(PlotID, species_new) %>%
  summarise(value = sum(value)) %>%
  filter(value != 0) %>%
  group_by(species_new) %>%
  mutate(noobs = n()) %>%
  filter(noobs > 1) %>%
  select(-noobs) %>%
  pivot_wider(names_from = species_new, values_fill = 0)

write_xlsx(spe, 'meta\\species_data_for_analysis.xlsx')

cs <- colSums((read_xlsx('orig_data\\spe.xlsx')[-1]) != 0)
sp_counts <- tibble(species = names(cs),
                    noobs = cs)

twin <- twinspan(spe)

head <- read_xlsx('orig_data\\head.xlsx') %>%
  filter(PlotID %in% selected[[1]]) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
  mutate(twin4 = cut(twin, 4),
         twin3 = cut(twin, 3),
         twin2 = cut(twin, 2),
         twin1 = cut(twin, 1))

head %>% 
  mutate(as_tibble(st_coordinates(.))) %>% 
  as_tibble() %>% 
  dplyr::select(PlotID, twin4, twin3, twin2, twin1, X, Y) %>% 
  write_xlsx('meta\\header_data.xlsx')

# -------------------------------------------------------------------------
# syntables
for(i in c('twin1', 'twin2', 'twin3', 'twin4')){
  cl <- head[[i]]
  write_xlsx(syn_sortable(spe[-1], cl), paste0('Syntable_', i, '.xlsx'))
}

# -------------------------------------------------------------------------
# ordination comes here 
cap <- capscale(sqrt(spe) ~ 1, distance = 'bray', sqrt.dist = T)
cap_lab <- paste0('PCo', 1:2, ' (', round((cap$CA$eig/cap$tot.chi)[1:2]*100, 2), '%)')
sp_sc <- rownames_to_column(as.data.frame(scores(cap)$species), 'species') %>%
  mutate(species_abb = vegtools::make_names(species))
a <- bind_cols(head, scores(cap, choices = 1:3)$sites) %>%
  mutate(y = st_coordinates(.)[,2]) %>%
  ggplot(aes(MDS1, MDS2)) +
  labs(x = cap_lab[1], y = cap_lab[2]) +
  geom_point(aes(colour = factor(twin2), size = y)) +
  scale_colour_discrete(name = 'Twinspan group') +
  scale_size_continuous('Latitude') +
  theme_bw() +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_blank())
a
b <- sp_sc %>% left_join(sp_counts %>%
                           mutate(species = gsub('_T0', '', species),
                                  species = gsub('_T1', '', species))) %>%
  filter(noobs > 30) %>%
  ggplot(aes(MDS1, MDS2)) +
  labs(x = cap_lab[1], y = cap_lab[2]) +
  geom_text_repel(aes(label = species_abb)) +
  geom_point(size = 3, shape = 3) + theme_bw()
b

ggsave('outputs\\Pinpir_ordination_3-div.svg', a+b, height = 8, width =15)
