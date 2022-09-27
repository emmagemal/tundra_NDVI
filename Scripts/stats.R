## Statistics for Group 6 Arctic Ecosystems Project
## Emma Gemal

### Library ----
library(tidyverse)
library(lmtest)  # for Breusch-Pagan test for homoskedasticity 


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


### Analysis Data ---- 
ndvi.rich <-full_join(plot.full, rich)
head(ndvi.rich)

# fixing the order of elevations to be accurate to the height (= for plotting)
ndvi.rich <- ndvi.rich %>% 
                mutate(elevation_cat = as.factor(elevation_cat)) %>% 
                mutate(elevation_cat = factor(elevation_cat, 
                                              levels = c("L", "LM", "M", "MH", "H")))
str(ndvi.rich)


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
# for all models, adding I(elevation_m-527) means 527 = elevation 1, so it doesn't estimate from
    # 0 m a.s.l. because that's not where our area is 

# NDVI ~ temperature, precipitation and community 
lm2 <- lm(NDVI ~ I(elevation_m-527) + community + site, data = ndvi.rich)
plot(lm2int)  # some potential outliers, but won't remove 
shapiro.test(resid(lm2int))   # normally distributed, p > 0.05 
bptest(lm2int)   # no heteroskedasticity 
summary(lm2int)

lm2int <- lm(NDVI ~ I(elevation_m-527)*community*site, data = ndvi.rich)
plot(lm2int2)  # some potential outliers, but won't remove 
shapiro.test(resid(lm2int2))   # normally distributed, p > 0.05 
bptest(lm2int2)   # no heteroskedasticity 
summary(lm2int2)

# seeing which structure is better
AIC(lm2, lm2int)  # lm2int is best 

# NDVI ~ temperature, precipitation, community and richness
lm3 <- lm(NDVI ~ I(elevation_m-527)*community*site + sp_richness, data = ndvi.rich)
plot(lm3)  # also a few potential outliers
shapiro.test(resid(lm3))   # normal, p > 0.05 
bptest(lm3)   # no heteroskedasticity 

lm3int <- lm(NDVI ~ I(elevation_m-527)*community*site*sp_richness, data = ndvi.rich)
plot(lm3int)  # also a few potential outliers
shapiro.test(resid(lm3int))   # normal, p > 0.05 
bptest(lm3int)   # no heteroskedasticity 

# NDVI ~ temperature, precipitation, community and height 
lm4 <- lm(NDVI ~ I(elevation_m-527)*community*site + avg_height, data = ndvi.rich)
plot(lm4)  # also a few potential outliers
shapiro.test(resid(lm4)) # normal, p > 0.05
bptest(lm4)   # no heteroskedasticity  

lm4int <- lm(NDVI ~ I(elevation_m-527)*community*site*avg_height, data = ndvi.rich)
plot(lm4int)  # also a few potential outliers
shapiro.test(resid(lm4int)) # NOT normal, p < 0.05, don't use!!
bptest(lm4int)   # no heteroskedasticity  

# NDVI ~ temperature, precipitation, community, richness and height
lm5 <- lm(NDVI ~ I(elevation_m-527)*community*site + sp_richness + avg_height, data = ndvi.rich)
plot(lm5)  # also a few potential outliers
shapiro.test(resid(lm5)) # normal, p > 0.05
bptest(lm5)   # no heteroskedasticity  

lm5int <- lm(NDVI ~ I(elevation_m-527)*community*site*sp_richness + avg_height, data = ndvi.rich)
plot(lm5int)  # also a few potential outliers
shapiro.test(resid(lm5int)) # normal, p > 0.05
bptest(lm5int)   # no heteroskedasticity  

lm5int2 <- lm(NDVI ~ I(elevation_m-527)*community*site*avg_height*sp_richness, 
             data = ndvi.rich)
plot(lm5int2)  # also a few potential outliers
shapiro.test(resid(lm5int2)) # NOT normal, p < 0.05, don't use!!
bptest(lm5int2)   # no heteroskedasticity 

# NDVI ~ richness and community
lm6 <- lm(NDVI ~ community*sp_richness, data = ndvi.rich)
plot(lm6)  # also a few potential outliers
shapiro.test(resid(lm6)) # normal, p > 0.05, just 
bptest(lm6)   # no heteroskedasticity 

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
lm8 <- lm(NDVI ~ avg_height*community, data = ndvi.rich)
plot(lm8)  # also a few potential outliers
shapiro.test(resid(lm8)) # normal, p > 0.05 
bptest(lm8)  # no heteroskedasticity

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
AIC(lm2, lm2int, lm3, lm3int, lm4, lm4int, lm5, lm5int, lm5int2, lm6, lm7, lm7int, lm8, 
    lm9int, lm10, lm11, nullmod1)
  # lowest = lm4int (DF = 25 though)
  # next best (simpler, lower DF) = lm3, lm4, lm2int (all within 2 units)

summary(lm4int)  # too many variables 

summary(lm4)  # height included, nearly significant (but still not)
summary(lm3)  # richness included, nearly significant (but still not)
summary(lm2int)  # no richness nor height 

anova(lm4, lm3)
anova(lm4, lm2int)
anova(lm3, lm2int)

## interpretation ## 
anova(lm2int)   # to get overview results, because it's a 3-way ANCOVA 
summary(lm2int)   # to get detailed results 

# interactions = change in slope for that community or change in mean/ intercept for that site
    # they are the effects when controlling for e.g. community or site 

## ANCOVA results --
# elevation alone does not explain NDVI (p = 0.489), but community type does (p < 2.2e-16)
# site also significantly explains NDVI (there is a difference between sites, p = 5.40e-10)
# the interaction between elevation and community is significant (p = 0.0178), meaning the
    # effect of elevation on NDVI depends on the community type 
# but the effect of elevation does NOT differ by site (p = 0.832)
# the effect of each community significantly differs by site (p = 0.170) = different relationships
    # not seen in summary() = don't mention it 
# but when accounting for elevation, there is no difference in the relationships for each community
    # across the 2 sites (p = 0.612)

## Model results --
# in K, NDVI is significantly higher in short shrubs than cryptograms (C int = 0.5397, 
    # S int = 0.7268, p = 1.25e-5)
# in K, NDVI is the highest in tall shrubs, but not significantly greater than short shrubs (SE is
    # too big = encompasses short shrub value), but it's significantly higher than C (T int = 0.7294,
    # p = 1.03e-5)
# NDVI is not significantly lower or higher in N (S: p = 0.0903, T: p = 0.592)

# there is no effect of elevation on NDVI for short shrubs in K (p = 0.390), and no effect in N 
    # either (p = 0.864) --> no difference in relationship between sites either 
# there IS a difference in the relationship between elevation and NDVI for tall shrubs between
    # the sites though, with K showing a significant decline with elevation (slope = -7.043e-4, 
    # p = 0.0164), but N not (SE > estimate, p = 0.448)
# for cryptogams, there is no effect of elevation but there is a trend towards an increase
    # (slope = 3.579e-4, SE = high (2.024e-4), p = 0.0809), but there is a difference between
    # sites, with N having a lower NDVI overall (K int = 0.5397, N int = 0.4191, p = 0.00417)

# adjusted R2 = 0.6828
