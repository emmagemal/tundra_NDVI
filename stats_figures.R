## Statistics and Figures for Group 6 Arctic Ecosystems Project
## Emma Gemal

### Library ----
library(tidyverse)
library(vegan)
library(labdsv)

## Workflow 


### Data Exploration ----
sp.full <- read.csv("Data/sp_data.csv", header = T, strip.white = T)
plot.full <- read.csv("Data/plot_data.csv", header = T)

str(sp.full)
str(plot.full)

## Seeing if any names are inconsistent 
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
              mutate(community = ifelse(community == "L", "C", community)) %>% 
              mutate(plot_nr = as.factor(plot_nr)) %>% 
              mutate(elevation_cat = ifelse(elevation_cat == "ML", "LM", elevation_cat)) %>% 
              mutate(elevation_cat = ifelse(elevation_cat == "ML ", "LM", elevation_cat)) 

unique(sp.full$species_id) 
unique(sp.full$community)
unique(sp.full$elevation_cat)
str(sp.full)

plot.full <- plot.full %>% 
                mutate(plot_nr = as.factor(plot_nr)) %>% 
                mutate(community = ifelse(community == "L", "C", community)) %>% 
                mutate(elevation_cat = ifelse(elevation_cat == "ML", "LM", elevation_cat)) %>% 
                mutate(elevation_cat = ifelse(elevation_cat == "ML ", "LM", elevation_cat))
                
unique(plot.full$community)
unique(plot.full$elevation_cat)
str(plot.full)

### Species Richness Calculation ----
sp.full <- sp.full %>% 
              group_by(site, elevation_cat, community, plot_nr) %>% 
              mutate(sp_richness = length(unique(species_id))) %>% 
              ungroup()

str(sp.full)

rich <- sp.full %>% 
          dplyr::select(site, elevation_cat, community, plot_nr, sp_richness) %>% 
          distinct()
rich

### Initial Plots ----
# combining with plot data
ndvi.rich <-full_join(plot.full, rich)
head(ndvi.rich)

# fixing the order of elevations to be accurate to the height (= for plotting)
ndvi.rich <- ndvi.rich %>% 
                mutate(elevation_cat = as.factor(elevation_cat)) %>% 
                mutate(elevation_cat = factor(elevation_cat, 
                                              levels = c("L", "LM", "M", "MH", "H")))
str(ndvi.rich)

# NDVI ~ elevation + community + precipitation   <-- will probably use this for the results
ggplot(ndvi.rich, aes(x = elevation_m, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = community)) +
  geom_point(aes(color = community)) +
  facet_wrap(~site, scales = "free_x")

# NDVI ~ elevation + community + precipitation   
ggplot(ndvi.rich, aes(x = elevation_m, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = site)) +
  geom_point(aes(color = site)) +
  facet_wrap(~community, scales = "free_x")

# NDVI ~ community + elevation (cat) + site
ggplot(ndvi.rich, aes(x = elevation_cat, y = NDVI)) +
  geom_boxplot(aes(fill = community)) +
  facet_wrap(~site)

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

# NDVI ~ height + community + site
ggplot(ndvi.rich, aes(x = avg_height, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = site)) + 
  geom_point(aes(color = site)) +
  facet_wrap(~community, scales = "free_x")


### Moss:Lichen Ratio ----
## Looking at ratio of moss to lichen in cryptogram plots 
crypto <- sp.full %>% 
            filter(sp_group == "lichen" | sp_group == "moss and liverwort") %>% 
            filter(coverage > 0)

crypto <- crypto %>% 
            dplyr::select(site, elevation_cat, community, plot_nr, sp_group, coverage) %>% 
            pivot_wider(names_from = sp_group, values_from = coverage) 

crypto <- crypto %>%  
            mutate(lichen = replace_na(lichen, 0)) %>% 
            mutate(`moss and liverwort` = replace_na(`moss and liverwort`, 0)) %>% 
            mutate(moss_liverwort = `moss and liverwort`) %>% 
            dplyr::select(!`moss and liverwort`)

# calculating the ratio (normalized)
crypto <- crypto %>%  
            mutate(ratio = (moss_liverwort-lichen)/(moss_liverwort+lichen))  
    # -1 = more lichen, +1 = more moss 

# adding back NDVI
crypto <- left_join(crypto, plot.full, by = c("site", "elevation_cat", "community", "plot_nr")) 
crypto <- crypto %>% 
            dplyr::select(site, elevation_cat, community, plot_nr, ratio, NDVI) %>% 
            filter(community == "C")  # only interested in this community 


# plotting 
ggplot(crypto, aes(x = site, y = ratio)) + 
  geom_boxplot()  # seems to be similar, but slightly higher ratio in K than N (more moss)

ggplot(crypto, aes(x = ratio, y = NDVI)) +
  stat_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~site)  # higher NDVI in K, more plots with moss dominance in K than in N 

# testing for significance 
t.test(ratio ~ site, data = crypto)  # not a significant difference though between sites 


# calculating average ratio
  # by site only
crypto_sum <- crypto %>% 
                group_by(site) %>% 
                summarize(avg_ratio = mean(ratio))

ggplot(crypto_sum, aes(x = site, y = avg_ratio)) + 
  geom_point()

  # by elevation and site 
crypto_sum2 <- crypto %>% 
                  group_by(site, elevation_cat) %>% 
                  summarize(avg_ratio = mean(ratio))

ggplot(crypto_sum2, aes(x = site, y = avg_ratio)) + 
  geom_jitter(width = 0.1)



### NMDS ----
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




