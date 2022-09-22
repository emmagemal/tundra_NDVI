## Statistics and Figures for Group 6 Arctic Ecosystems Project
## Emma Gemal

### Library ----
library(tidyverse)
library(vegan)
library(labdsv)

### Data Exploration ----
sp.full <- read.csv("Data/sp_data.csv", header = T, strip.white = T)
plot.full <- read.csv("Data/plot_data.csv", header = T)

str(sp.full)
str(plot.full)

unique(sp.full$species_id)   # 'vanlig styvstarr' == 'styvstarr' == 'styvstar' 
                             # 'moss 1' == "moss1 
unique(plot.full$elevation_cat)  # 'LM' == 'ML' == 'ML '
unique(sp.full$community)   # remove C (same as L)

unique(plot.full$community)
unique(plot.full$elevation_cat)

sp.full <- sp.full %>% 
              mutate(species_id = ifelse(species_id == "moss1", "moss 1", species_id)) %>% 
              mutate(species_id = ifelse(species_id == "styvstarr", 
                                         "vanlig styvstarr", species_id)) %>% 
              mutate(species_id = ifelse(species_id == "styvstar", 
                                         "vanlig styvstarr", species_id)) %>% 
              mutate(community = ifelse(community == "C", "L", community)) %>% 
              mutate(plot_nr = as.factor(plot_nr)) %>% 
              mutate(elevation_cat = ifelse(elevation_cat == "ML", "LM", elevation_cat)) %>% 
              mutate(elevation_cat = ifelse(elevation_cat == "ML ", "LM", elevation_cat)) 

unique(sp.full$species_id) 
unique(sp.full$community)
unique(sp.full$elevation_cat)
str(sp.full)

plot.full <- plot.full %>% 
                mutate(plot_nr = as.factor(plot_nr)) %>% 
                mutate(community = ifelse(community == "C", "L", community)) %>% 
                mutate(elevation_cat = ifelse(elevation_cat == "ML", "LM", elevation_cat)) %>% 
                mutate(elevation_cat = ifelse(elevation_cat == "ML ", "LM", elevation_cat))
                
unique(plot.full$community)
unique(plot.full$elevation_cat)
str(plot.full)

### Species Richness ----
sp.full <- sp.full %>% 
              group_by(site, elevation_cat, community, plot_nr) %>% 
              mutate(sp_richness = length(unique(species_id))) %>% 
              ungroup()

str(sp.full)

rich <- sp.full %>% 
          dplyr::select(site, elevation_cat, community, plot_nr, sp_richness) %>% 
          distinct()
rich

# combining with plot data
ndvi.rich <-full_join(plot.full, rich)

head(ndvi.rich)

### Initial Plots ----
# fixing the order of elevations to be accurate to the height (= for plotting)
ndvi_rich <- ndvi.rich %>% 
                mutate(elevation_cat = as.factor(elevation_cat)) %>% 
                mutate(elevation_cat = factor(elevation_cat, 
                                              levels = c("L", "LM", "M", "MH", "H")))


# NDVI ~ elevation + community + precipitation   <-- will probably use this for the results
ggplot(ndvi.rich, aes(x = elevation_m, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = community)) +
  geom_point(aes(color = community)) +
  facet_wrap(~site, scales = "free_x")

# NDVI ~ community + elevation (cat)
ggplot(ndvi.rich, aes(x = community, y = NDVI)) +
  geom_boxplot() +
  facet_wrap(~elevation_cat)

# NDVI ~ elevation + community (cont)
ggplot(ndvi.rich, aes(x = elevation_m, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = community, fill = community)) +
  geom_point(aes(color = community))


# NDVI ~ precipitation + community  
ggplot(ndvi.rich, aes(x = community, y = NDVI)) +
  geom_boxplot(aes(color = site ))

## To see if they correlate well 
  # NDVI ~ LAI
ggplot(ndvi.rich, aes(x = NDVI, y = LAI)) +   # they do, sexy 
  geom_point()
  # NDVI ~ fPAR
ggplot(ndvi.rich, aes(x = NDVI, y = fPAR)) +
  geom_point()
  # LAI ~ fPAR
ggplot(ndvi.rich, aes(x = LAI, y = fPAR)) +
  geom_point()


# NDVI ~ richness + site   <-- could use this for results, but not shown for each sp.
ggplot(ndvi.rich, aes(x = sp_richness, y = NDVI)) +
  stat_smooth(method = "lm") + 
  geom_point(aes(color = community)) + 
  facet_wrap(~site, scales = "free_x")

# NDVI ~ richness + site + community   <-- useful for results
ggplot(ndvi.rich, aes(x = sp_richness, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = community)) + 
  geom_point(aes(color = community)) + 
  facet_wrap(~site, scales = "free_x")


## NMDS
# making a new object for community analysis
matrix.sp <- sp.full %>% 
                dplyr::select(site, elevation_cat, plot_nr, community, 
                              species_id, sp_group, coverage) %>% 
                mutate(coverage = replace_na(coverage, 1)) %>% 
                unite("plot", 1:4, remove = F) %>% 
                mutate(species_id = ifelse(sp_group == "", species_id, sp_group)) %>% 
                group_by(plot) %>% 
                
                
matrix.sp

matrix.sp <- matrify(matrix.sp)

### Statistical Analysis ----




