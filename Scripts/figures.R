## Figures of the results for Group 6 Arctic Ecosystems Project
## Emma Gemal

### Library ----
library(tidyverse)


### Data ----
sp.full <- read.csv("Data/sp_data.csv", header = T, strip.white = T)
plot.full <- read.csv("Data/plot_data.csv", header = T)

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

rich %>% 
  group_by(elevation_cat, site, community) %>%
  summarise(no_rows = length(elevation_cat))   # checking we have all the plots 


### Moss:Lichen Ratio Calculation ----
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
            dplyr::select(site, elevation_cat, elevation_m, community, plot_nr, ratio, NDVI) %>% 
            filter(community == "C") %>%   # only interested in this community 
            mutate(precip = case_when(site == "K" ~ "wet",
                                      site == "N" ~ "dry")) %>% 
            mutate(precip = as.factor(precip)) %>% 
            mutate(precip = factor(precip, levels = c("wet", "dry")))

# coverage of moss and lichen for plotting 
Ccover <- sp.full %>% 
              filter(community == "C",
                     sp_group == "moss and liverwort" | sp_group == "lichen") %>% 
              na.omit() %>% 
              mutate(precip = case_when(site == "K" ~ "wet",
                                        site == "N" ~ "dry")) %>% 
              mutate(precip = as.factor(precip)) %>% 
              mutate(precip = factor(precip, levels = c("wet", "dry")))

### Plot Data ----
# combining with plot data
ndvi.rich <-full_join(plot.full, rich)
head(ndvi.rich)

# fixing the order of elevations to be accurate to the height (= for plotting)
ndvi.rich <- ndvi.rich %>% 
                mutate(elevation_cat = as.factor(elevation_cat)) %>% 
                mutate(elevation_cat = factor(elevation_cat, 
                                              levels = c("L", "LM", "M", "MH", "H"))) %>% 
                mutate(precip = case_when(site == "K" ~ "wet",
                                          site == "N" ~ "dry")) %>% 
                mutate(precip = as.factor(precip)) %>% 
                mutate(precip = factor(precip, levels = c("wet", "dry")))


### Initial Plots ----
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

# NDVI ~ richness + site 
ggplot(ndvi.rich, aes(x = sp_richness, y = NDVI)) +
  stat_smooth(method = "lm") + 
  geom_point(aes(color = community)) + 
  facet_wrap(~site, scales = "free_x")

# NDVI ~ richness + site + community   <-- useful for results
ggplot(ndvi.rich, aes(x = sp_richness, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = community)) + 
  geom_point(aes(color = community)) + 
  facet_wrap(~site, scales = "free")

# NDVI ~ richness + site + community  
ggplot(ndvi.rich, aes(x = sp_richness, y = NDVI)) +
  geom_boxplot(aes(color = community, fill = community), alpha = 0.5) +
  facet_wrap(~site, scales = "free_x")

# richness ~ site + community
ggplot(ndvi.rich, aes(x = site, y = sp_richness)) +
  geom_boxplot(aes(color = community, fill = community), alpha = 0.5)

# NDVI ~ height + community + site  <-- better representation, shows effect of height on NDVI
ggplot(ndvi.rich, aes(x = avg_height, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = site)) + 
  geom_point(aes(color = site)) +
  facet_wrap(~community, scales = "free_x")

# NDVI ~ height + community + site 
ggplot(ndvi.rich, aes(x = avg_height, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = community)) + 
  geom_point(aes(color = community)) + 
  facet_wrap(~site, scales = "free_x")

# NDVI ~ height
ggplot(ndvi.rich, aes(x = avg_height, y = NDVI)) +
  stat_smooth(method = "lm", aes(color = community)) +
  geom_point(aes(color = community))

# height ~ elevation + site + community (cat)
ggplot(ndvi.rich, aes(x = elevation_cat, y = avg_height)) +
  geom_boxplot(aes(color = community, fill = community), alpha = 0.5) +
  facet_wrap(~site)

# height ~ elevation + site + community (cont)
ggplot(ndvi.rich, aes(x = elevation_m, y = avg_height)) +
  stat_smooth(method = "lm", aes(color = community, fill = community)) +
  geom_point(aes(color = community)) +
  facet_wrap(~site)

# height ~ site + community
ggplot(ndvi.rich, aes(x = site, y = avg_height)) +
  geom_boxplot(aes(color = community, fill = community), alpha = 0.5)

# moss:lichen ratio ~ NDVI
ggplot(crypto, aes(x = ratio, y = NDVI)) +
  stat_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~site)  # higher NDVI in K, more plots with moss dominance in K than in N 

# coverage of moss and lichen
ggplot(Ccover, aes(x = sp_group, y = coverage)) +
  geom_boxplot() +
  facet_wrap(~site)


### Final Plots ----
# making a theme
theme_ndvi <- theme_bw() +
                theme(panel.grid = element_blank(),
                      axis.title.x = 
                        element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                      axis.title.y = 
                        element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# NDVI ~ elevation + community + precipitation - MAIN RESULT 
(plot_main <- ggplot(ndvi.rich, aes(x = elevation_m, y = NDVI)) +
                stat_smooth(method = "lm", aes(color = community, fill = community)) +
                geom_point(aes(color = community, shape = site)) +
                facet_wrap(~precip, scales = "free_x") +
                xlab("Elevation (m a.s.l.)") +
                ylab("NDVI") +
                theme_ndvi +
                theme(legend.key.size = unit(0.5, 'cm'),
                      legend.title = element_text(size = 10), 
                      legend.text = element_text(size = 8)) + 
                scale_shape_discrete(labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                     name = "Site") +
                scale_color_manual(values = c("#F4A460", "#A78290", "#4E7BBF"),
                                   name = "Community",
                                   labels = c("Cryptogams", "Short Shrubs", "Tall Shrubs"))+
                scale_fill_manual(values = c("#F4A460", "#A78290", "#4E7BBF"),
                                   name = "Community",
                                   labels = c("Cryptogams", "Short Shrubs", "Tall Shrubs")))

ggsave("Figures/NDVI_mainresults.png", plot = plot_main, width = 6, height = 4, units = "in")

# NDVI ~ richness + site + community 
(plot_rich <- ggplot(ndvi.rich, aes(x = sp_richness, y = NDVI)) +
                stat_smooth(method = "lm", aes(color = community, fill = community)) + 
                geom_point(aes(color = community, shape = site)) + 
                facet_wrap(~precip, scales = "free_x") +
                xlab("Elevation (m a.s.l.)") +
                ylab("Species Richness") +
                theme_ndvi +
                scale_shape_discrete(labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                     name = "Site") +
                scale_color_manual(values = c("#F4A460", "#A78290", "#4E7BBF"),
                                   name = "Community",
                                   labels = c("Cryptogams", "Short Shrubs", "Tall Shrubs"))+
                scale_fill_manual(values = c("#F4A460", "#A78290", "#4E7BBF"),
                                  name = "Community",
                                  labels = c("Cryptogams", "Short Shrubs", "Tall Shrubs")))

# NDVI ~ height + community + site 
(plot_height <- ggplot(ndvi.rich, aes(x = avg_height, y = NDVI)) +
                  stat_smooth(method = "lm", aes(color = site)) + 
                  geom_point(aes(color = site)) +
                  facet_wrap(~community, scales = "free_x") +
                  xlab("Average height (cm)") +
                  ylab("NDVI") +
                  theme_ndvi +
                  scale_color_manual(values = c("#9BCD9B", "#F4A460"),
                                     labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                     name = "Site") +
                  scale_fill_manual(values = c("#9BCD9B", "#F4A460"),
                                    labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                    name = "Site"))


# height ~ elevation + community + site 
(plot_height2 <- ggplot(ndvi.rich, aes(x = elevation_m, y = avg_height)) +
                    stat_smooth(method = "lm", aes(color = community, fill = community)) + 
                    geom_point(aes(color = community, shape = site)) +
                    facet_wrap(~precip, scales = "free_x") +
                    xlab("Elevation (m a.s.l.)") +
                    ylab("Average height (cm)") +
                    theme_ndvi +
                    scale_shape_discrete(labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                         name = "Site") +
                    scale_color_manual(values = c("#F4A460", "#A78290", "#4E7BBF"),
                                       name = "Community",
                                       labels = c("Cryptogams", "Short Shrubs", "Tall Shrubs"))+
                    scale_fill_manual(values = c("#F4A460", "#A78290", "#4E7BBF"),
                                      name = "Community",
                                      labels = c("Cryptogams", "Short Shrubs", "Tall Shrubs")))

ggsave("Figures/height.png", plot = plot_height2, width = 6, height = 4, units = "in")


# NDVI ~ moss:lichen 
(plot_ratio <- ggplot(crypto, aes(x = ratio, y = NDVI)) +
                  stat_smooth(method = "lm", aes(color = precip, fill = site)) +
                  geom_vline(xintercept = 0, color = "grey", linetype = 2, size = 0.2) +
                  geom_point(aes(color = precip)) +
                  facet_wrap(~precip) +
                  xlab("Moss:lichen ratio") +
                  ylab("NDVI") +
                  theme_ndvi +
                  theme(legend.position = "none") +
                  scale_color_manual(values = c("#9BCD9B", "#F4A460")) +
                  scale_fill_manual(values = c("#9BCD9B", "#F4A460")))

ggsave("Figures/moss-lichen_ratio.png", plot = plot_ratio, width = 5, height = 4, units = "in")

