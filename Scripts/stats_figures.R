## Statistics and Figures for Group 6 Arctic Ecosystems Project
## Emma Gemal

### Library ----
library(tidyverse)
library(vegan)
library(lmtest)  # for Breusch-Pagan test for homoskedasticity 


## Workflow 


### Data ----
sp.full <- read.csv("Data/sp_data.csv", header = T, strip.white = T)
plot.full <- read.csv("Data/plot_data.csv", header = T)

### Data Exploration ----
str(sp.full)
str(plot.full)

## Seeing if any names are inconsistent 
unique(sp.full$species_id)  
unique(plot.full$elevation_cat) 
unique(sp.full$community)   

unique(plot.full$community)
unique(plot.full$elevation_cat)
# looks good 


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
            dplyr::select(site, elevation_cat, elevation_m, community, plot_nr, ratio, NDVI) %>% 
            filter(community == "C")  # only interested in this community 


# plotting 
ggplot(crypto, aes(x = site, y = ratio)) + 
  geom_boxplot()  # seems to be similar, but slightly higher ratio in K than N (more moss)

ggplot(crypto, aes(x = ratio, y = NDVI)) +
  stat_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~site)  # higher NDVI in K, more plots with moss dominance in K than in N 

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




### Statistical Analysis ----
## Moss:Lichen Ratio -- 
shapiro.test(crypto$ratio)   # not normal itself 
t.test(ratio ~ site, data = crypto)  # not a significant difference though between sites 

shapiro.test(resid(lm(ratio ~ site*elevation_m, data = crypto)))  # normal residuals 
summary(lm(ratio ~ elevation_m*site, data = crypto))  # no significant differences 


## Linear models -- 
# NDVI ~ temperature and precipitation (irrespective of community)
lm1 <- lm(NDVI ~ elevation_m + site, data = ndvi.rich)
plot(lm1)  # some heteroskedasticity may be present 
shapiro.test(resid(lm1))  # normal, p > 0.05
bptest(lm1)   # no heteroskedasticity, but close 

lm1int <- lm(NDVI ~ elevation_m*site, data = ndvi.rich)
plot(lm1int)  # some heteroskedasticity may be present 
shapiro.test(resid(lm1int))  # normal, p > 0.05
bptest(lm1int)   # no heteroskedasticity 

# NDVI ~ temperature, precipitation and community 
lm2 <- lm(NDVI ~ elevation_m + community + site, data = ndvi.rich)
plot(lm2)  # some potential outliers, but won't remove 
shapiro.test(resid(lm2))   # normally distributed, p > 0.05 
bptest(lm2)   # no heteroskedasticity 

lm2int <- lm(NDVI ~ elevation_m + community + site, data = ndvi.rich)
plot(lm2int)  # some potential outliers, but won't remove 
shapiro.test(resid(lm2int))   # normally distributed, p > 0.05 
bptest(lm2int)   # no heteroskedasticity 

lm2int2 <- lm(NDVI ~ elevation_m*community + site, data = ndvi.rich)
plot(lm2int2)  # some potential outliers, but won't remove 
shapiro.test(resid(lm2int2))   # normally distributed, p > 0.05 
bptest(lm2int2)   # no heteroskedasticity 

lm2int3 <- lm(NDVI ~ elevation_m + community*site, data = ndvi.rich)
plot(lm2int3)  # some potential outliers, but won't remove 
shapiro.test(resid(lm2int3))   # normally distributed, p > 0.05 
bptest(lm2int3)   # no heteroskedasticity 

# NDVI ~ temperature, precipitation, community and richness
lm3 <- lm(NDVI ~ elevation_m + community + site + sp_richness, data = ndvi.rich)
plot(lm3)  # some potential outliers, but won't remove 
shapiro.test(resid(lm3))   # normally distributed, p > 0.05 
bptest(lm3)   # no heteroskedasticity 

lm3int <- lm(NDVI ~ elevation_m*community*site*sp_richness, data = ndvi.rich)
plot(lm3int)  # also a few potential outliers
shapiro.test(resid(lm3int))   # normal, p > 0.05 
bptest(lm3int)   # no heteroskedasticity 

lm3int2 <- lm(NDVI ~ elevation_m*community*site + sp_richness, data = ndvi.rich)
plot(lm3int2)  # also a few potential outliers
shapiro.test(resid(lm3int2))   # normal, p > 0.05 
bptest(lm3int2)   # no heteroskedasticity 

# NDVI ~ temperature, precipitation, community and height 
lm4 <- lm(NDVI ~ elevation_m + community + site + avg_height, data = ndvi.rich)
plot(lm4)  # some potential outliers, but won't remove 
shapiro.test(resid(lm4))   # normally distributed, p > 0.05 
bptest(lm4)   # no heteroskedasticity 

lm4int <- lm(NDVI ~ elevation_m*community*site*avg_height, data = ndvi.rich)
plot(lm4int)  # also a few potential outliers
shapiro.test(resid(lm4int)) # NOT normal, p < 0.05 = DON'T USE AS IS
bptest(lm4int)   # no heteroskedasticity though 

hist(ndvi.rich$avg_height)  # very skewed, would need to use poisson in the model 
  # only fix if significant = a variable worth including 

# NDVI ~ temperature, precipitation, community, richness and height
lm5 <- lm(NDVI ~ elevation_m + community + site + sp_richness + avg_height, data = ndvi.rich)
plot(lm5)  # some potential outliers, but won't remove 
shapiro.test(resid(lm5))   # normally distributed, p > 0.05 
bptest(lm5)   # no heteroskedasticity 

lm5int <- lm(NDVI ~ elevation_m*community*site*sp_richness*avg_height, data = ndvi.rich)
plot(lm5int)  # also a few potential outliers
shapiro.test(resid(lm5int)) # NOT normal, p < 0.05 = DON'T USE AS IS
bptest(lm5int)   # no heteroskedasticity though 

# NDVI ~ richness and community
lm6 <- lm(NDVI ~ sp_richness + community, data = ndvi.rich)
plot(lm6)  # some potential outliers, but won't remove 
shapiro.test(resid(lm6))   # normally distributed, p > 0.05 barely 
bptest(lm6)   # no heteroskedasticity 

lm6int <- lm(NDVI ~ community*sp_richness, data = ndvi.rich)
plot(lm6int)  # also a few potential outliers
shapiro.test(resid(lm6int)) # normal, p > 0.05 
bptest(lm6int)   # no heteroskedasticity 

# NDVI ~ richness and height 
lm7 <- lm(NDVI ~ sp_richness + avg_height, data = ndvi.rich)
plot(lm7)  # some potential outliers, but won't remove 
shapiro.test(resid(lm7))   # normally distributed, p > 0.05  
bptest(lm7)   # no heteroskedasticity 

lm7int <- lm(NDVI ~ sp_richness*avg_height, data = ndvi.rich)
plot(lm7int)  # also a few potential outliers
shapiro.test(resid(lm7int)) # normal, p > 0.05 
bptest(lm7int)  # no heteroskedasticity

# NDVI ~ height and community
lm8 <- lm(NDVI ~ avg_height + community, data = ndvi.rich)
plot(lm8)  # some potential outliers, but won't remove 
shapiro.test(resid(lm8))   # normal, p > 0.05  
bptest(lm8)   # YES heteroskedasticity :(

lm8int <- lm(NDVI ~ avg_height*community, data = ndvi.rich)
plot(lm8int)  # also a few potential outliers
shapiro.test(resid(lm8int)) # normal, p > 0.05 
bptest(lm8int)  # no heteroskedasticity

# NDVI ~ richness, height and community 
lm9 <- lm(NDVI ~ sp_richness + avg_height + community, data = ndvi.rich)
plot(lm9)  # some potential outliers, but won't remove 
shapiro.test(resid(lm9))   # normally distributed, p > 0.05  
bptest(lm9)   # no heteroskedasticity 

lm9int <- lm(NDVI ~ sp_richness*avg_height*community, data = ndvi.rich)
plot(lm9int)  # also a few potential outliers
shapiro.test(resid(lm9int)) # NOT normal, p < 0.05 
bptest(lm9int)  # no heteroskedasticity

# Null model 
nullmod1 <- lm(NDVI ~ 1, data = ndvi.rich)

# Comparison of models
AIC(lm1, lm1int, lm2, lm2int, lm2int2, lm2int3, lm3, lm3int, lm3int2, lm4, lm5, lm6, lm6int, 
    lm7, lm7int, lm8int, lm9, nullmod1)
  # lowest = lm3int, has very high DF (25) though 
  # best simple models (lower DF) = lm2int2, lm2int3, lm5 or lm4 
      # (>2 units better than lm3 = next best) 

# richness and height were NOT better predictors of NDVI than elevation or site 

summary(lm3int)  # richness has no significant effect on any relationships = omit as variable
  # also too complex to interpret 
summary(lm4)
summary(lm5)  # being able to have richness AND height in model = kinda nice
  # also chose this due to SLIGHTLY higher adjusted R2 value 

# estimate = effect of predictor for a 1 unit increase in NDVI 

# average NDVI when x = 0 is 0.545 (intercept)
# as elevation increases, NDVI increases (slope = 1.087e-4)
    # but this is not significant (p = 0.1892)
# short shrubs have a higher NDVI than cryptogams and tall shrubs (communityS = 0.545 + 0.186 = 0.731)
  # is significantly greater than cryptogams and tall shrubs overall (p = 3.77e-16)
# tall shrubs only have a higher NDVI than cryptogams (communityT = 0.545 + 0.0499)
# NDVI is significantly lower in Nissonjokk (dry) than in Katterjokk 
    # (siteN is negative, p = 7.36e-6)
# 

  