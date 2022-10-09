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


### adding richness to see if it correlates ###
crypt.rich <- ndvi.rich %>% 
                filter(community == "C")
crypto2 <- left_join(crypto, crypt.rich)
str(crypto2)

ggplot(crypto2, aes(x = sp_richness, y = ratio)) +
  geom_point() +
  facet_wrap(~site, scales = "free_x")

summary(lm(ratio ~ sp_richness*site, data = crypto2))  # no effect of ratio of lichens on richness

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

shapiro.test(resid(lm(NDVI ~ ratio, data = crypto)))  # normal
summary(lm(NDVI ~ ratio, data = crypto))  # no significant differences 


## Basic Stats --
anova(lm(NDVI ~ elevation_cat, data = ndvi.rich))
summary(lm(NDVI ~ elevation_m, data = ndvi.rich))  # NDVI doesn't increase with elevation
                                                      # p = 0.698, R2 = 0.0017! SO LOW 

summary(lm(avg_height ~ elevation_m, data = ndvi.rich))  # height doesn't change with elevation
                                                          # but a trend towards a decline in height
                                                          # higher up (p = 0.0695, slope = -0.019 cm)

summary(lm(sp_richness ~ elevation_m, data = ndvi.rich))  # richness declines significantly with 
                                                            # elevation (p = 0.0260, slope = 0.0099)

t.test(NDVI ~ site, data = ndvi.rich)  # NDVI is significantly lower in Nissonjokk (dry)
                                          # p = 3.214e-5, DF = 82, t = 4.4011
                                          # mean K = 0.669, mean N = 0.568
 
t.test(avg_height ~ site, data = ndvi.rich)  # height is not different between the sites
                                                # p = 0.8604, DF = 85, t = -0.176

t.test(sp_richness ~ site, data = ndvi.rich)  # richness is significantly lower in Katterjokk (wet)
                                                # p = 3.747e-9, DF = 79, t = -6.633
                                                # mean K = 13.24 spp, mean N = 17.89 spp 


## Multiple Linear Regression -- 
# for all models, adding I(elevation_m-527) means 527 = elevation 1, so it doesn't estimate from
    # 0 m a.s.l. because that's not where our area is 

# NDVI ~ temperature, precipitation and community 
lm2 <- lm(NDVI ~ I(elevation_m-527) + community + site, data = ndvi.rich)
plot(lm2)  # some potential outliers, but won't remove 
shapiro.test(resid(lm2))   # normally distributed, p > 0.05 
bptest(lm2)   # no heteroskedasticity 
summary(lm2)

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

# Null model 
nullmod1 <- lm(NDVI ~ 1, data = ndvi.rich)


# Comparison of models
AIC(lm2, lm2int, lm3, lm3int, lm4, lm4int, lm5, lm5int, lm5int2, nullmod1)
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
anova(lm2int)
summary(lm2int)   # to get detailed results 

# site and community only
aovlm <- aov(NDVI ~ community*site, data = ndvi.rich)
TukeyHSD(aovlm)

# splitting by site to get direct result of community 
aovlmK <- ndvi.rich %>% filter(site == "K") 
aovlmK <- aov(NDVI ~ community, data = aovlmK)
aovlmN <- ndvi.rich %>% filter(site == "N")
aovlmN <- aov(NDVI ~ community, data = aovlmN)

TukeyHSD(aovlmK)
TukeyHSD(aovlmN)

# splitting by community to get direct result of site 
Ctest <- ndvi.rich %>% filter(community == "C") 
t.test(NDVI ~ site, data = Ctest)
Stest <- ndvi.rich %>% filter(community == "S")
t.test(NDVI ~ site, data = Stest)
Ttest <- ndvi.rich %>% filter(community == "T")
t.test(NDVI ~ site, data = Ttest)

TukeyHSD(aovlmK)
TukeyHSD(aovlmN)



## HOW TO INTERPRET:
# elevation = slope for C
# community = intercept diff for S and T compared to C
# siteN = intercept diff for C in site N 
# elevation:community = difference in slope for S and T compared to C (C slope + other slope)
# community:site = difference in intercept for S and T compared to C (C int + other int)
# elevation:community:site = difference in slope in site N for each community compared to C in K

# difference usually calculated as base + new (+ new)
    # where base = intercept estimate or elevation estimate 
    # and new is the value for a different community, and (+ new) is for another site 

# p-values = whether the difference described is significant 


## Model results --
# in Katterjokk, mean NDVI is significantly higher in short shrubs than cryptograms (C int = 0.5397, 
    # S int = 0.7268, p = 1.25e-5)
# in Katterjokk, NDVI is the highest in tall shrubs, but not significantly greater than short 
    # shrubs (SE is too big = encompasses short shrub value), but it's significantly higher than 
    # cryptogams (T int = # 0.7294, p = 1.03e-5)
# mean NDVI is not significantly different (not lower nor higher) in Nissonjokk for short nor tall
    # shrubs (S: p = 0.0903, T: p = 0.592), but mean NDVI is significantly lower in Nissonjokk for
    # cryptogams (p = 0.00417, int = 0.4191)

# elevation had no effect on cryptogam NDVI in Katterjokk, but there is a trend towards an increase 
    # (slope = 3.579e-4, SE = high (2.024e-4), p = 0.0809), and the same is true for Nissonjokk 
    # since there was no significant interaction effect between elevation and siteN (p = 0.690)
# for short shrubs, elevation also had no effect on NDVI in Katterjokk since the slope is not 
    # significantly different from that of cryptogams (p = 0.390, slope = 1.097e-4), and this was 
    # also the case in Nissonjokk (p = 0.864, slope = 4.504e-5)
# for tall shrubs, increased elevation significantly reduced NDVI in Katterjokk, since there is a
    # significantly different effect compared to cryptogams and the slope is negative (p = 0.0164,
    # slope = -3.464e-4), but for Nissonjokk tall shrubs did not significantly differ in their
    # relationship with elevation to cryptogams and therefore there is no effect of elevation on 
    # NDVI in Nissonjokk (p = 0.44806, slope = -5.88e-5)

# adjusted R2 = 0.6828



## Effects of richness and height --
# NDVI ~ richness and community
lm6 <- lm(NDVI ~ community*sp_richness, data = ndvi.rich)
plot(lm6)  # also a few potential outliers
shapiro.test(resid(lm6)) # normal, p > 0.05 
bptest(lm6)   # no heteroskedasticity 

# NDVI ~ richness, community, site
lm6int <- lm(NDVI ~ community*sp_richness*site, data = ndvi.rich)
plot(lm6int)  # also a few potential outliers
shapiro.test(resid(lm6int)) # NOT normal, p < 0.05, makes sense (need poisson)
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
lm8 <- lm(NDVI ~ avg_height*community, data = ndvi.rich)
plot(lm8)  # also a few potential outliers
shapiro.test(resid(lm8)) # normal, p > 0.05 
bptest(lm8)  # no heteroskedasticity

# NDVI ~ height, community, site
lm8int <- lm(NDVI ~ avg_height*community*site, data = ndvi.rich)
plot(lm8int)  # also a few potential outliers
shapiro.test(resid(lm8int)) # NOT normal, p > 0.05 
bptest(lm8int)  # no heteroskedasticity

# NDVI ~ richness, height and community 
lm9int <- lm(NDVI ~ sp_richness*avg_height*community, data = ndvi.rich)
plot(lm9int)  # also a few potential outliers
shapiro.test(resid(lm9int)) # NOT normal, p < 0.05 
bptest(lm9int)  # no heteroskedasticity

# NDVI ~ richness only
lm10 <- lm(NDVI ~ sp_richness, data = ndvi.rich)
plot(lm10)  # some potential outliers, but won't remove 
shapiro.test(resid(lm10))   # NOT normally distributed, p < 0.05, it's count data duh 
bptest(lm10)  # no heteroskedasticity though

# NDVI ~ height only 
lm11 <- lm(NDVI ~ avg_height, data = ndvi.rich)
plot(lm11)  # some potential outliers, but won't remove 
shapiro.test(resid(lm11))   # NOT normally distributed, p > 0.05  
bptest(lm11)  # no heteroskedasticity though 

AIC(lm3, lm3int, lm4, lm4int, lm5, lm5int, lm5int2, lm6, lm6int, lm7, lm7int, lm8, 
    lm8int, lm9int, lm10, lm11, nullmod1)
# lm4int = the best, but has 25 DF 
# lm8int = next best (13 DF = simpler)

anova(lm4int)
summary(lm4int)
# with elevation, height has some effects in a few places (particularly on tall shrubs)
summary(lm8int)
# height doesn't have any effect without elevation = not worth including 


## Looking at effects for only T -- 
# NDVI ~ height, community, site
ndvi.T <- ndvi.rich %>% 
              filter(community == "T")

# with elevation 
lm4int.T <- lm(NDVI ~ avg_height*site*I(elevation_m-527), data = ndvi.T)
summary(lm4int.T) 

# height ~ elevation*site 
lm.h1 <- lm(avg_height ~ I(elevation_m-527)*site, data = ndvi.T)
plot(lm.h1)  # also a few potential outliers
shapiro.test(resid(lm.h1)) # NOT normal, p < 0.05 
bptest(lm.h1)  # heteroskedasticity not present 
summary(lm.h1)

hist(ndvi.T$avg_height)  # kinda skewed

glm.h1 <- glm(avg_height ~ I(elevation_m-527)*site, family = Gamma(link = log), data = ndvi.T)
summary(glm.h1)  # no effect of anything other than community


t.test(avg_height ~ site, data = ndvi.T)
shapiro.test(resid(lm(avg_height ~ site, data = ndvi.T))) # NOT normal, p < 0.05 
bptest(lm(avg_height ~ site, data = ndvi.T))  # heteroskedasticity not present 

wilcox.test(avg_height ~ site, data = ndvi.T)   # non-parametric 


lm.h2 <- lm(avg_height ~ I(elevation_m-527), data = ndvi.T)
plot(lm.h2)  # also a few potential outliers
shapiro.test(resid(lm.h2)) # NOT normal, p < 0.05 
bptest(lm.h2)  # heteroskedasticity not present 

glm.h2 <- glm(avg_height ~ I(elevation_m-527), family = Gamma(link = log), data = ndvi.T)
summary(glm.h2)  

glm.null <- glm(avg_height ~ 1, family = Gamma(link = log), data = ndvi.T)

AIC(glm.h1, glm.h2, glm.null)  # best is glm.h1

Anova(glm.h1)
summary(glm.h1)  


## Looking at effects for only C -- 
# NDVI ~ height, community, site
ndvi.C <- ndvi.rich %>% 
            filter(community == "C")

# with elevation 
lm4int.C <- lm(NDVI ~ avg_height*site*I(elevation_m-527), data = ndvi.C)
summary(lm4int.C)   # no significant effect of height


# height ~ elevation*site 
lm.h1C <- lm(avg_height ~ I(elevation_m-527)*site, data = ndvi.C)
plot(lm.h1C)  # also a few potential outliers
shapiro.test(resid(lm.h1C)) # normal, p > 0.05 
bptest(lm.h1C)  # heteroskedasticity not present 

hist(ndvi.C$avg_height)  # kinda skewed, not too bad 

glm.h1C <- glm(avg_height ~ I(elevation_m-527)*site, Gamma(link = log), data = ndvi.C)
summary(glm.h1C)  


t.test(avg_height ~ site, data = ndvi.C)
shapiro.test(resid(lm(avg_height ~ site, data = ndvi.C))) # NOT normal, p < 0.05 
bptest(lm(avg_height ~ site, data = ndvi.C))  # heteroskedasticity IS present 

wilcox.test(avg_height ~ site, data = ndvi.C)   # non-parametric 


lm.h2C <- lm(avg_height ~ I(elevation_m-527), data = ndvi.C)
plot(lm.h2C)  # also a few potential outliers
shapiro.test(resid(lm.h2C)) # NOT normal, p < 0.05 
bptest(lm.h2C)  # heteroskedasticity not present 

glm.h2C <- glm(avg_height ~ I(elevation_m-527), Gamma(link = log), data = ndvi.C)
summary(glm.h2C)  

glm.nullC <- glm(avg_height ~ 1, Gamma(link = log), data = ndvi.C)

AIC(glm.h1C, glm.h2C, glm.nullC)  # null is best/ not worse than the others 
