## Temperature Investigation for Group 6 Arctic Ecosystems Project
## Emma Gemal

### Library ----
library(tidyverse)
# library(DescTools)  # only needed for ColToGrey function


# Loading the data 
temp <- read.csv2("Data/temperature.csv", header = T)

str(temp)
unique(temp$elev_cat)

temp <- temp %>% 
          mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M")) %>% 
          mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>% 
          mutate(temp = as.numeric(temp)) %>% 
          mutate(elev_cat = if_else(elev_cat == "ML", "LM", elev_cat)) %>% 
          mutate(elev_cat = as.factor(elev_cat)) %>% 
          mutate(elev_cat = factor(elev_cat, levels = c("L", "LM", "M", "MH", "H"))) %>% 
          mutate(precip = case_when(site == "K" ~ "wet",
                                    site == "N" ~ "dry")) %>% 
          mutate(precip = as.factor(precip)) %>% 
          mutate(precip = factor(precip, levels = c("wet", "dry")))

str(temp)

# making elevation into a numeric variable 
temp <- temp %>%   
          mutate(elev_m = case_when(site == "K" & elev_cat == "L" ~ 530,
                                    site == "K" & elev_cat == "LM" ~ 571,
                                    site == "K" & elev_cat == "M" ~ 622,
                                    site == "K" & elev_cat == "MH" ~ 702,
                                    site == "K" & elev_cat == "H" ~ 764,
                                    site == "N" & elev_cat == "L" ~ 527,
                                    site == "N" & elev_cat == "LM" ~ 594,
                                    site == "N" & elev_cat == "M" ~ 661,
                                    site == "N" & elev_cat == "MH" ~ 716,
                                    site == "N" & elev_cat == "H" ~ 820))

# making a summarized dataset
temp_sum <- temp %>% 
              group_by(elev_cat, site, date) %>% 
              mutate(n = length(unique(time))) %>% 
              summarize(n = length(date),
                        mean = mean(temp),
                        max = max(temp),
                        min = min(temp),
                        sd = sd(temp)) %>% 
              ungroup()

temp_sum <- temp_sum %>%   # making elevation into a numeric variable 
              mutate(elev_m = case_when(site == "K" & elev_cat == "L" ~ 530,
                                        site == "K" & elev_cat == "LM" ~ 571,
                                        site == "K" & elev_cat == "M" ~ 622,
                                        site == "K" & elev_cat == "MH" ~ 702,
                                        site == "K" & elev_cat == "H" ~ 764,
                                        site == "N" & elev_cat == "L" ~ 527,
                                        site == "N" & elev_cat == "LM" ~ 594,
                                        site == "N" & elev_cat == "M" ~ 661,
                                        site == "N" & elev_cat == "MH" ~ 716,
                                        site == "N" & elev_cat == "H" ~ 820))

# averaging for plotting
temp_sum.avg <- temp_sum %>% 
                  group_by(elev_cat, site) %>% 
                  summarize(mean2 = mean(mean),
                            max2 = mean(max),
                            min2 = mean(min),
                            sd.mean = sd(mean),
                            sd.max = sd(max),
                            sd.min = sd(min),
                            n = sqrt(n),
                            se.mean = (sd.mean/n),
                            se.max = (sd.max/n),
                            se.min = (sd.min/n),
                            elev_m = mean(elev_m)) %>% 
                  summarize(mean = mean(mean2),
                            max = mean(max2),
                            min = mean(min2),
                            sd.mean = mean(sd.mean),
                            sd.max = mean(sd.max),
                            sd.min = mean(sd.min),
                            se.mean = mean(sd.mean),
                            se.max = mean(se.max),
                            se.min = mean(se.min),
                            elev_m = mean(elev_m))

temp_sum.avg2 <- temp_sum.avg %>% 
                    dplyr::select(elev_cat, elev_m, site, mean, min, max)
temp_sum.error <- temp_sum.avg %>% 
                    dplyr::select(elev_cat, elev_m, site, se.mean, se.max, se.min)

# making the objects longer for plotting 
temp_sum2 <- temp_sum %>% 
               pivot_longer(cols = c("mean", "max", "min"), names_to = "stat", values_to = "temp") %>% 
               mutate(precip = case_when(site == "K" ~ "wet",
                                         site == "N" ~ "dry")) %>% 
               mutate(precip = as.factor(precip)) %>% 
               mutate(precip = factor(precip, levels = c("wet", "dry")))

temp_sum.avg2 <- temp_sum.avg2 %>% 
                    pivot_longer(cols = c("mean", "max", "min"), 
                                 names_to = "stat", values_to = "temp")
temp_sum.error <- temp_sum.error %>% 
                    pivot_longer(cols = c("se.mean", "se.min", "se.max"),
                                 names_to = "stat", names_prefix = "se.", values_to = "error") 

temp.avg.long <- left_join(temp_sum.avg2, temp_sum.error)


### Plots ----
## Initial plots 
# date and time vs. temperature
plot(temp$date_time, temp$temp)

# date and time vs. temperature + elevation 
plot(temp ~ date_time, data = temp, pch = 20, cex = 0.8, col = elev_cat);
legend("topleft", legend = levels(temp$elev_cat), fill = 1:5, cex = 0.8)


# Actual plots
# making a theme
theme_ndvi <- theme_bw() +
                theme(panel.grid = element_blank(),
                      axis.title.x = 
                        element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                      axis.title.y = 
                        element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# scatterplot with mean across whole period
(temp_plot <- ggplot(temp.avg.long, aes(elev_m, temp)) +
                geom_point(aes(color = stat)) +
                stat_smooth(method = "lm", aes(color = stat)) +
                facet_wrap(~site, scales = "free_x") +
                theme_ndvi)

# boxplots showing distribution across whole period    <--- using this one 
(temp_box <- ggplot(temp_sum2, aes(elev_cat, temp)) +
                geom_boxplot(aes(fill = stat, color = stat), alpha = 0.5) +
                facet_wrap(~precip, scales = "free") +
                xlab("Elevation level") +
                ylab("Daily temperature (˚C)") +
                theme_ndvi +
                theme(legend.title = element_blank()) +
                scale_color_manual(values = c("#F4A460", "#62946B", "#255090"),
                                   labels = c("Maximum", "Mean", "Minimum")) +
                scale_fill_manual(values = c("#F4A460", "#62946B", "#255090"),
                                   labels = c("Maximum", "Mean", "Minimum")))

ggsave("Figures/temp_boxplot.png", plot = temp_box, width = 6, height = 4, units = "in")

# non-split boxplots
(temp_box2 <- ggplot(temp, aes(elev_cat, temp)) +
                geom_boxplot(aes(fill = elev_cat, color = elev_cat), alpha = 0.5) +
                facet_wrap(~precip, scales = "free") +
                xlab("Elevation level") +
                ylab("Daily temperature (˚C)") +
                theme_ndvi +
                theme(legend.position = "none") +
                scale_color_manual(values = c("L" = "#F4A460", "LM" = "#D38579", "M" = "#A78290", 
                                              "MH" = "#7A7EA8", "H" = "#4E7BBF")) +
                scale_fill_manual(values = c("L" = "#F4A460", "LM" = "#D38579", "M" = "#A78290", 
                                             "MH" = "#7A7EA8", "H" = "#4E7BBF")))

ggsave("Figures/temp_boxplot2.png", plot = temp_box2, width = 6, height = 4, units = "in")

# alt color scheme 
(temp_box3 <- ggplot(temp, aes(elev_cat, temp)) +
                geom_boxplot(aes(fill = site, color = site), alpha = 0.5) +
                facet_wrap(~precip, scales = "free") +
                xlab("Elevation level") +
                ylab("Daily temperature (˚C)") +
                theme_ndvi +
                theme(legend.position = "none") +
                scale_color_manual(values = c("#78A678", "#F4A460")) +
                scale_fill_manual(values = c("#78A678", "#F4A460")))

ggsave("Figures/temp_boxplot_site.png", plot = temp_box3, width = 6, height = 4, units = "in")


# hourly measurements for whole period 
(temp_hour <- ggplot(temp, aes(x = date_time, y = temp)) +
                geom_point(aes(color = elev_cat)) +
                facet_wrap(~site, scales = "free_x") +
                theme_ndvi + 
                scale_color_manual(values = c("L" = "#F4A460", "LM" = "#D38579", "M" = "#A78290", 
                                              "MH" = "#7A7EA8", "H" = "#4E7BBF"),
                                   name = "Elevation",
                                   labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High")))


### Statistic ----
# overall (ANOVA)
lm1 <- lm(temp ~ elev_cat*site, data = temp)
shapiro.test(resid(lm1))   # not normal 
plot(lm1)  # but looks pretty good... 

anova(lm1)  # significant difference in T between elevations, p = 0.01218 
summary(lm1)  # not between all elevations though 

# overall (continuous variable)
lm2 <- lm(temp ~ elev_m*site, data = temp)
shapiro.test(resid(lm2))  # not normal 
plot(lm2)  # looks ok... 
hist(temp$temp)   # looks pretty normal... keeping it! 

summary(lm2)  # significant decline in T with elevation, p = 7.45e-6, slope = -0.00539 

# no interaction
lm2.no <- lm(temp ~ elev_m + site, data = temp)
summary(lm2.no) # significant decline with elevation, p = 1.46e-08, slope = -0.0047001
                  # site also significant, p = 0.00109, slope = 0.5071741

AIC(lm2, lm2.no)  # lm2.no (no interaction) = best 


# mean T 
lm3 <- lm(mean ~ elev_m*site, data = temp_sum)
summary(lm3)  # significant decline in the mean T, p = 0.00192, slope = -0.005398 

# max T 
lm4 <- lm(max ~ elev_m*site, data = temp_sum)
summary(lm4)  # no decline in the max T, p = 0.498

# min T 
lm5 <- lm(min ~ elev_m*site, data = temp_sum)
summary(lm5)  # no decline in the min T, p = 0.818




