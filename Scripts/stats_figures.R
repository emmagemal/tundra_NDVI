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
# NDVI ~ temperature, precipitation and community 
lm2 <- lm(NDVI ~ elevation + community + site, data = ndvi.rich)
lm2int <- lm(NDVI ~ elevation_m*community*site, data = ndvi.rich)
plot(lm2int)  # some potential outliers, but won't remove 
shapiro.test(resid(lm2int))   # normally distributed, p > 0.05 
bptest(lm2int)   # no heteroskedasticity 
summary(lm2int)

lm2int2 <- lm(NDVI ~ elevation_m*community + site*community, data = ndvi.rich)
plot(lm2int2)  # some potential outliers, but won't remove 
shapiro.test(resid(lm2int2))   # normally distributed, p > 0.05 
bptest(lm2int2)   # no heteroskedasticity 
summary(lm2int2)

# seeing which structure is better
AIC(lm2, lm2int, lm2int2)  # lm2int2 is better, so it doesn't matter if relationship with elevation
                           # is allowed to differ by site (also seen in plots)

# NDVI ~ temperature, precipitation, community and richness
lm3int <- lm(NDVI ~ elevation_m*community*sp_richness + site*community*sp_richness, data = ndvi.rich)
plot(lm3int)  # also a few potential outliers
shapiro.test(resid(lm3int))   # normal, p > 0.05 
bptest(lm3int)   # no heteroskedasticity 
summary(lm3int)

lm3int2 <- lm(NDVI ~ elevation_m*community + site*community + sp_richness, data = ndvi.rich)
plot(lm3int2)  # also a few potential outliers
shapiro.test(resid(lm3int2))   # normal, p > 0.05 
bptest(lm3int2)   # no heteroskedasticity 
summary(lm3int2)

# NDVI ~ temperature, precipitation, community and height 
lm4int <- lm(NDVI ~ elevation_m*community*avg_height + site*community*avg_height, data = ndvi.rich)
plot(lm4int)  # also a few potential outliers
shapiro.test(resid(lm4int)) # NOT normal, p < 0.05 = DON'T USE AS IS
bptest(lm4int)   # no heteroskedasticity though 

hist(ndvi.rich$avg_height)  # very skewed, would need to use poisson in the model 
  # only fix if significant = a variable worth including 

lm4int2 <- lm(NDVI ~ elevation_m*community + site*community + avg_height, data = ndvi.rich)
plot(lm4int2)  # also a few potential outliers
shapiro.test(resid(lm4int2)) # normal, p > 0.05
bptest(lm4int2)   # no heteroskedasticity  

# NDVI ~ temperature, precipitation, community, richness and height
lm5int <- lm(NDVI ~ elevation_m*community + site*community + sp_richness + avg_height, 
             data = ndvi.rich)
plot(lm5int)  # also a few potential outliers
shapiro.test(resid(lm5int)) # normal, p > 0.05
bptest(lm5int)   # no heteroskedasticity  

lm5int2 <- lm(NDVI ~ elevation_m*community*sp_richness + site*community*sp_richness + avg_height, 
             data = ndvi.rich)
plot(lm5int2)  # also a few potential outliers
shapiro.test(resid(lm5int2)) # normal, p > 0.05
bptest(lm5int2)   # no heteroskedasticity  

lm5int3 <- lm(NDVI ~ elevation_m*community*avg_height + site*community*avg_height + sp_richness, 
             data = ndvi.rich)
plot(lm5int3)  # also a few potential outliers
shapiro.test(resid(lm5int3)) # NOT normal, p < 0.05, don't use!!
bptest(lm5int3)   # no heteroskedasticity 

lm5int4 <- lm(NDVI ~ elevation_m*community*avg_height*sp_richness + 
                site*community*avg_height*sp_richness, data = ndvi.rich)
plot(lm5int4)  # also a few potential outliers
shapiro.test(resid(lm5int4)) # NOT normal, p < 0.05, don't use!!
bptest(lm5int4)   # no heteroskedasticity 

# NDVI ~ richness and community
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
lm8int <- lm(NDVI ~ avg_height*community, data = ndvi.rich)
plot(lm8int)  # also a few potential outliers
shapiro.test(resid(lm8int)) # normal, p > 0.05 
bptest(lm8int)  # no heteroskedasticity

# NDVI ~ richness, height and community 
lm9int <- lm(NDVI ~ sp_richness*avg_height*community, data = ndvi.rich)
plot(lm9int)  # also a few potential outliers
shapiro.test(resid(lm9int)) # NOT normal, p < 0.05 
bptest(lm9int)  # no heteroskedasticity

# NDVI ~ richness only
lm10 <- glm(NDVI ~ sp_richness, data = ndvi.rich)
plot(lm10)  # some potential outliers, but won't remove 
shapiro.test(resid(lm10))   # NOT normally distributed, p < 0.05, it's count data duh 
bptest(lm10)  # no heteroskedasticity though

# NDVI ~ height only 
lm11 <- lm(NDVI ~ avg_height, data = ndvi.rich)
plot(lm11)  # some potential outliers, but won't remove 
shapiro.test(resid(lm11))   # NOT normally distributed, p > 0.05  
bptest(lm11)  # no heteroskedasticity though 

# Null model 
nullmod1 <- lm(NDVI ~ 1, data = ndvi.rich)

# Comparison of models
AIC(lm3int, lm3int2, lm4int, lm4int2, lm5int, lm5int2, lm5int3, lm5int4, lm6int, 
    lm7, lm7int, lm8int, lm9int, lm10, lm11, nullmod1)
  # lowest = lm4int (232.09), lm3int, lm5int2, lm5int3 (values of 230.32 - 230.96) 
      # DF = 19, 19, 20, 20 = one of the first 2 will be best 
  # next best (simpler, DF = 11 and 12) = lm5int or lm4int2 

summary(lm3int)  # richness has no significant effect on any relationships = omit as variable?
  # note that richness itself (NDVI ~ richness) is a strong predictor, but not in the presence of
  # these other variables we are interested in 
summary(lm4int)  # avg_height = just an extra explanatory variable, maybe it shouldn't be  
                    # interacting with the other predictors 
summary(lm4int2) # simpler, perhaps better 
summary(lm5int)  # since richness has no effect = go with lm4int2 


## interpretation ## 
summary(lm4int2)
anova(lm4int2)
# this model = a weird type of ANCOVA (continuous ~ continuous + categorical...) 

# Katterjokk: 
  # short shrubs have a higher NDVI than cryptogams (= 0.38) and tall shrubs in Katterjokk, 
      # (communityS = 0.38 + 0.33 = 0.71) and this is significant (p = 0.00636*)
  # tall shrubs only have a higher NDVI than cryptogams in K (communityT = 0.38 + 0.32 = 0.70)
      # this is also significant (p = 0.1721)

  # for cryptogams, there is an increase (slope = 0.000299) in NDVI with elevation in both sites
      # (interaction: p = 0.217)
  # for short shrubs, there is no difference in NDVI with elevation (interaction: p = 0.120)
  # for tall shrubs, there is a decrease (slope = 0.000299 + (-0.00039) = -0.000091) in NDVI
      # as elevation increases (interaction: p = 0.0440*)

# Nissonjokk:
  # NDVI of cryptogams is significantly lower in Nissonjokk (dry) than in Katterjokk (wet) 
      # (siteN = 0.38 + (-0.135) = 0.245, p = 3.04e-7*)
  # NDVI of short shrubs is higher in Nissonjokk (dry) than in Katterjokk (wet)
      # (siteN = 0.38 + 0.094 = 0.474, p = 0.00727*)
  # NDVI of tall shrubs is lower in N 

# NDVI increases as average vegetation height increases (slope = 0.0025)
    # this is significant (p = 0.0395*)


0.38 + 0.094
  