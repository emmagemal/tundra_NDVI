### Library ----
library(tidyverse)
library(vegan)

### Data ---- 
matrix.sp <- read.csv("Data/nmds_matrix.csv", header = T)


### Separating matrices by community ----
str(matrix.sp)
matrix.sp[is.na(matrix.sp)] <- 0

matrix.c <- matrix.sp %>% 
              filter(grepl("_C", plot))
matrix.s <- matrix.sp %>% 
              filter(grepl("_S", plot))
matrix.t <- matrix.sp %>% 
              filter(grepl("_T", plot))

# changing plot column to numeric
matrix.c <- matrix.c %>% 
              mutate(plot_num = 1:30) %>% 
              dplyr::select(plot, plot_num, site, elevation, 
                            betula.nana:vanlig.styvstarr)  # reordering
matrix.s <- matrix.s %>% 
              mutate(plot_num = 1:30) %>% 
              dplyr::select(plot, plot_num, site, elevation, betula.nana:vanlig.styvstarr)

matrix.t <- matrix.t %>% 
              mutate(plot_num = 1:30) %>% 
              dplyr::select(plot, plot_num, site, elevation, betula.nana:vanlig.styvstarr)

# saving for making ellipsoids 
plots.c <- matrix.c %>% 
            dplyr::select(plot, plot_num, site, elevation)
plots.s <- matrix.s %>% 
            dplyr::select(plot, plot_num, site, elevation)
plots.t <- matrix.t %>% 
            dplyr::select(plot, plot_num, site, elevation)

# removing categorical plot data 
matrix.c <- matrix.c %>% 
              dplyr::select(plot_num, betula.nana:vanlig.styvstarr)
matrix.s <- matrix.s %>% 
              dplyr::select(plot_num, betula.nana:vanlig.styvstarr)
matrix.t <- matrix.t %>% 
              dplyr::select(plot_num, betula.nana:vanlig.styvstarr)

### CRYPTOGAMS ----  
# Looking at how many axes to extract 
c1.mds <- metaMDS(matrix.c, distance = "bray", k = 1)
c2.mds <- metaMDS(matrix.c, distance = "bray", k = 2)
c3.mds <- metaMDS(matrix.c, distance = "bray", k = 3)
c4.mds <- metaMDS(matrix.c, distance = "bray", k = 4)
c5.mds <- metaMDS(matrix.c, distance = "bray", k = 5)
c6.mds <- metaMDS(matrix.c, distance = "bray", k = 6)
c7.mds <- metaMDS(matrix.c, distance = "bray", k = 7)

screeC <-
  cbind(rbind(1,2,3,4,5,6,7),rbind(c1.mds$stress,c2.mds$stress,c3.mds$stress,c4.mds$
                                     stress,c5.mds$stress,c6.mds$stress,c7.mds$stress))
plot(screeC)
# 4 axes seem reasonable

c.nmds <- metaMDS(matrix.c, distance = "bray", k = 2, autotransform = TRUE, trymax = 500, 
                  sratmax = 0.999)  
c.nmds
# dimensions = 2
# stress = 0.147

ordiplot(c.nmds)
orditorp(c.nmds, display = "species", col = "red", air = 0.01)

plots.c2 <- plots.c %>% 
              dplyr::select(elevation, site) %>% 
              mutate(elev_m = case_when(site == "K" & elevation == "L" ~ 530,
                                        site == "K" & elevation == "LM" ~ 571,
                                        site == "K" & elevation == "M" ~ 622,
                                        site == "K" & elevation == "MH" ~ 702,
                                        site == "K" & elevation == "H" ~ 764,
                                        site == "N" & elevation == "L" ~ 527,
                                        site == "N" & elevation == "LM" ~ 594,
                                        site == "N" & elevation == "M" ~ 661,
                                        site == "N" & elevation == "MH" ~ 716,
                                        site == "N" & elevation == "H" ~ 820))
predictors <- envfit(c.nmds, plots.c2, permutations = 999, na.rm = TRUE)
predictors

## Plotting NMDS
# extracting NMDS scores (x and y coordinates)
data.scores.c <- as.data.frame(scores(c.nmds))
species_scores <- as.data.frame(scores(c.nmds, "species"))
species_scores$species <- rownames(species_scores)

data.scores.c$site <- plots.c$site
data.scores.c$elevation <- plots.c$elevation

pred_coord <- as.data.frame(scores(predictors, "factors")) * ordiArrowMul(predictors)
row.names.site <- c("siteK", "siteN")
row.names.elev <- c("elevationL", "elevationLM", "elevationM", "elevationMH", "elevationH")
pred_coord_elev <- pred_coord[!(row.names(pred_coord) %in% row.names.site),]
pred_coord_site <- pred_coord[!(row.names(pred_coord) %in% row.names.elev),]


(gg <- ggplot(data = data.scores.c, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores.c, aes(colour = site), size = 3, alpha = 0.5) + 
  scale_colour_manual(values = c("orange", "steelblue"))  + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = pred_coord_elev, size =1, alpha = 0.5, colour = "grey30") +
    geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
              alpha = 0.5) +
  geom_text(data = pred_coord_elev, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(pred_coord_elev)) + 
    geom_polygon(data = df_ell.c1, aes(x = NMDS1, y = NMDS2, group = group,
                                       color = group, fill = group), alpha = 0.2, 
                 size = 0.5, linetype = 1) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")))


# SITE
NMDS.c1 <- data.frame(MDS1 = c.nmds$points[,1], MDS2 = c.nmds$points[,2], 
                  group = data.scores.c$site)

NMDS.c1$group <- as.factor(NMDS.c1$group)
NMDS.c1.mean <- aggregate(NMDS.c1[,1:2], list(group = NMDS.c1$group), "mean")

# making the ellipsoids
ord.c1 <- ordiellipse(c.nmds, data.scores.c$site, label = T, conf = 0.95)

# function for ellipses
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}  # run from here

df_ell.c1 <- data.frame()   # run from here (this side)

for(g in levels(NMDS.c1$group)){
  df_ell.c1 <- rbind(df_ell.c1, 
                  cbind(as.data.frame(with(NMDS.c1[NMDS.c1$group==g,],
                                           veganCovEllipse(ord.c1[[g]]$cov,ord.c1[[g]]$center,
                                                           ord.c1[[g]]$scale)))
                        ,group=g))
}  # run from here 

(nmds.plot.c1 <- ggplot(data.scores.c, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.c1, aes(x = NMDS1, y = NMDS2, group = group,
                                                    color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = site, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_color_manual(values = c("#9BCD9B", "#F4A460"),
                                       labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                       name = "Site") +
                    scale_shape_discrete(labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                         name = "Site") +
                    scale_fill_manual(values = c("#9BCD9B", "#F4A460"),
                                      labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                      name = "Site"))

ggsave("Figures/NMDS_c_site.png", plot = nmds.plot.c1, width = 5, height = 4, units = "in")

# ELEVATION
# extracting NMDS scores (x and y coordinates)
NMDS.c2 <- data.frame(MDS1 = c.nmds$points[,1], MDS2 = c.nmds$points[,2], 
                  group = data.scores.c$elevation)

NMDS.c2$group <- as.factor(NMDS.c2$group)
NMDS.c2.mean = aggregate(NMDS.c2[,1:2], list(group = NMDS.c2$group), "mean")

# making the ellipsoids - ELEVATION
ord.c2 <- ordiellipse(c.nmds, data.scores.c$elevation, label = T, conf = 0.95)

df_ell.c2 <- data.frame()

for(g in levels(NMDS.c2$group)){
  df_ell.c2 <- rbind(df_ell.c2, 
                  cbind(as.data.frame(with(NMDS.c2[NMDS.c2$group==g,],
                                           veganCovEllipse(ord.c2[[g]]$cov,ord.c2[[g]]$center,
                                                           ord.c2[[g]]$scale)))
                        ,group=g))
}

(nmds.plot.c2 <- ggplot(data.scores.c, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.c2, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = elevation, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_shape_discrete(labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                         name = "Site") +
                    scale_fill_manual(values = c("L" = "#F4A460", "LM" = "#D38579", "M" = "#A78290", 
                                                 "MH" = "#7A7EA8", "H" = "#4E7BBF"),
                                      name = "Elevation",
                                      labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High")) +
                    scale_color_manual(values = c("L" = "#F4A460", "LM" = "#D38579", "M" = "#A78290", 
                                                  "MH" = "#7A7EA8", "H" = "#4E7BBF"),
                                       name = "Elevation",
                                       labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High")))

ggsave("Figures/NMDS_c_elev.png", plot = nmds.plot.c2, width = 5, height = 4, units = "in")


### SHORT SHRUBS ----  
# Looking at how many axes to extract 
s1.mds <- metaMDS(matrix.s, distance = "bray", k = 1)
s2.mds <- metaMDS(matrix.s, distance = "bray", k = 2)
s3.mds <- metaMDS(matrix.s, distance = "bray", k = 3)
s4.mds <- metaMDS(matrix.s, distance = "bray", k = 4)
s5.mds <- metaMDS(matrix.s, distance = "bray", k = 5)
s6.mds <- metaMDS(matrix.s, distance = "bray", k = 6)
s7.mds <- metaMDS(matrix.s, distance = "bray", k = 7)

screeS <-
  cbind(rbind(1,2,3,4,5,6,7),rbind(s1.mds$stress,s2.mds$stress,s3.mds$stress,s4.mds$
                                     stress,s5.mds$stress,s6.mds$stress,s7.mds$stress))
plot(screeS)
# 3-4 axes seem reasonable

s.nmds <- metaMDS(matrix.s, distance = "bray", k = 2, autotransform = TRUE, trymax = 500, 
                  sratmax = 0.999)  
s.nmds
# dimensions = 2
# stress = 0.164

## Plotting NMDS
# extracting NMDS scores (x and y coordinates)
data.scores.s <- as.data.frame(scores(s.nmds))
data.scores.s$site <- plots.s$site
data.scores.s$elevation <- plots.s$elevation

# SITE
NMDS.s1 <- data.frame(MDS1 = s.nmds$points[,1], MDS2 = s.nmds$points[,2], 
                      group = data.scores.s$site)

NMDS.s1$group <- as.factor(NMDS.s1$group)
NMDS.s1.mean <- aggregate(NMDS.s1[,1:2], list(group = NMDS.s1$group), "mean")

# making the ellipsoids
ord.s1 <- ordiellipse(s.nmds, data.scores.s$site, label = T, conf = 0.95)

df_ell.s1 <- data.frame()
for(g in levels(NMDS.s1$group)){
  df_ell.s1 <- rbind(df_ell.s1, 
                  cbind(as.data.frame(with(NMDS.s1[NMDS.s1$group==g,],
                                           veganCovEllipse(ord.s1[[g]]$cov,ord.s1[[g]]$center,
                                                           ord.s1[[g]]$scale)))
                        ,group=g))
}

(nmds.plot.s1 <- ggplot(data.scores.s, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.s1, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = site, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_color_manual(values = c("#9BCD9B", "#F4A460"),
                                       labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                       name = "Site") +
                    scale_shape_discrete(labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                         name = "Site") +
                    scale_fill_manual(values = c("#9BCD9B", "#F4A460"),
                                      labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                      name = "Site"))

ggsave("Figures/NMDS_s_site.png", plot = nmds.plot.s1, width = 5, height = 4, units = "in")

# ELEVATION
# extracting NMDS scores (x and y coordinates)
NMDS.s2 <- data.frame(MDS1 = s.nmds$points[,1], MDS2 = s.nmds$points[,2], 
                      group = data.scores.s$elevation)

NMDS.s2$group <- as.factor(NMDS.s2$group)
NMDS.s2.mean = aggregate(NMDS.s2[,1:2], list(group = NMDS.s2$group), "mean")

# making the ellipsoids - ELEVATION
ord.s2 <- ordiellipse(s.nmds, data.scores.s$elevation, label = T, conf = 0.95)

df_ell.s2 <- data.frame()
for(g in levels(NMDS.s2$group)){
  df_ell.s2 <- rbind(df_ell.s2, 
                     cbind(as.data.frame(with(NMDS.s2[NMDS.s2$group==g,],
                                              veganCovEllipse(ord.s2[[g]]$cov,ord.s2[[g]]$center,
                                                              ord.s2[[g]]$scale)))
                           ,group=g))
}

(nmds.plot.s2 <- ggplot(data.scores.s, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.s2, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = elevation, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_shape_discrete(labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                         name = "Site") +
                    scale_fill_manual(values = c("L" = "#F4A460", "LM" = "#D38579", "M" = "#A78290", 
                                                 "MH" = "#7A7EA8", "H" = "#4E7BBF"),
                                      name = "Elevation",
                                      labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High")) +
                    scale_color_manual(values = c("L" = "#F4A460", "LM" = "#D38579", "M" = "#A78290", 
                                                  "MH" = "#7A7EA8", "H" = "#4E7BBF"),
                                       name = "Elevation",
                                       labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High")))

ggsave("Figures/NMDS_s_elev.png", plot = nmds.plot.s2, width = 5, height = 4, units = "in")


### TALL SHRUBS ----
# Looking at how many axes to extract 
t1.mds <- metaMDS(matrix.t, distance = "bray", k = 1)
t2.mds <- metaMDS(matrix.t, distance = "bray", k = 2)
t3.mds <- metaMDS(matrix.t, distance = "bray", k = 3)
t4.mds <- metaMDS(matrix.t, distance = "bray", k = 4)
t5.mds <- metaMDS(matrix.t, distance = "bray", k = 5)
t6.mds <- metaMDS(matrix.t, distance = "bray", k = 6)
t7.mds <- metaMDS(matrix.t, distance = "bray", k = 7)

screeT <-
  cbind(rbind(1,2,3,4,5,6,7), rbind(t1.mds$stress,t2.mds$stress,t3.mds$stress,t4.mds$
                                      stress,t5.mds$stress,t6.mds$stress,t7.mds$stress))
plot(screeT)
# 4-5 axes seem reasonable

t.nmds <- metaMDS(matrix.t, distance = "bray", k = 2, autotransform = TRUE, trymax = 500, 
                  sratmax = 0.999)  
t.nmds
# dimensions = 2
# stress = 0.145

## Plotting NMDS
# extracting NMDS scores (x and y coordinates)
data.scores.t <- as.data.frame(scores(t.nmds))
data.scores.t$site <- plots.t$site
data.scores.t$elevation <- plots.t$elevation

# SITE
NMDS.t1 <- data.frame(MDS1 = t.nmds$points[,1], MDS2 = t.nmds$points[,2], 
                      group = data.scores.t$site)

NMDS.t1$group <- as.factor(NMDS.t1$group)
NMDS.t1.mean <- aggregate(NMDS.t1[,1:2], list(group = NMDS.t1$group), "mean")

# making the ellipsoids
ord.t1 <- ordiellipse(t.nmds, data.scores.t$site, label = T, conf = 0.95)

df_ell.t1 <- data.frame() 
for(g in levels(NMDS.t1$group)){
  df_ell.t1 <- rbind(df_ell.t1, 
                     cbind(as.data.frame(with(NMDS.t1[NMDS.t1$group==g,],
                                              veganCovEllipse(ord.t1[[g]]$cov,ord.t1[[g]]$center,
                                                              ord.t1[[g]]$scale)))
                           ,group=g))
}

(nmds.plot.t1 <- ggplot(data.scores.t, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.t1, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = site, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_color_manual(values = c("#9BCD9B", "#F4A460"),
                                       labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                       name = "Site") +
                    scale_shape_discrete(labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                         name = "Site") +
                    scale_fill_manual(values = c("#9BCD9B", "#F4A460"),
                                      labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                      name = "Site"))

ggsave("Figures/NMDS_t_site.png", plot = nmds.plot.t1, width = 5, height = 4, units = "in")

# ELEVATION
# extracting NMDS scores (x and y coordinates)
NMDS.t2 <- data.frame(MDS1 = t.nmds$points[,1], MDS2 = t.nmds$points[,2], 
                      group = data.scores.t$elevation)

NMDS.t2$group <- as.factor(NMDS.t2$group)
NMDS.t2.mean = aggregate(NMDS.t2[,1:2], list(group = NMDS.t2$group), "mean")

# making the ellipsoids - ELEVATION
ord.t2 <- ordiellipse(t.nmds, data.scores.t$elevation, label = T, conf = 0.95)

df_ell.t2 <- data.frame()
for(g in levels(NMDS.t2$group)){
  df_ell.t2 <- rbind(df_ell.t2, 
                     cbind(as.data.frame(with(NMDS.t2[NMDS.t2$group==g,],
                                              veganCovEllipse(ord.t2[[g]]$cov,ord.t2[[g]]$center,
                                                              ord.t2[[g]]$scale)))
                           ,group=g))
}

(nmds.plot.t2 <- ggplot(data.scores.t, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.t2, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = elevation, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_shape_discrete(labels = c("Katterjokk (wet)", "Nissonjokk (dry)"),
                                         name = "Site") +
                    scale_fill_manual(values = c("L" = "#F4A460", "LM" = "#D38579", "M" = "#A78290", 
                                                 "MH" = "#7A7EA8", "H" = "#4E7BBF"),
                                      name = "Elevation",
                                      labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High")) +
                    scale_color_manual(values = c("L" = "#F4A460", "LM" = "#D38579", "M" = "#A78290", 
                                                  "MH" = "#7A7EA8", "H" = "#4E7BBF"),
                                       name = "Elevation",
                                       labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High")))

ggsave("Figures/NMDS_t_elev.png", plot = nmds.plot.t2, width = 5, height = 4, units = "in")


### ANOSIM test ----
# testing if we have significantly different communities
## CRYPTOGAMS --
# Site
ano.c1 <- anosim(matrix.c, plots.c$site, distance = "bray", permutations = 9999)
ano.c1

# Elevation
ano.c2 <- anosim(matrix.c, plots.c$elevation, distance = "bray", permutations = 9999)
ano.c2

## SHORT SHRUBS --
# Site
ano.s1 <- anosim(matrix.s, plots.s$site, distance = "bray", permutations = 9999)
ano.s1

# Elevation
ano.s2 <- anosim(matrix.s, plots.s$elevation, distance = "bray", permutations = 9999)
ano.s2

## TALL SHRUBS --
# Site
ano.t1 <- anosim(matrix.t, plots.t$site, distance = "bray", permutations = 9999)
ano.t1

# Elevation
ano.t2 <- anosim(matrix.t, plots.t$elevation, distance = "bray", permutations = 9999)
ano.t2



## Q for Emil: is it ok to plot an NMDS with > 2 dimensions in 2D? (axis 1 ~ axis 2)
  # am I losing too much info/ how would I pick what axes to show? 
  # if I can get it to converge with 2 dimensions, is this worth chosing over more despite the
    # scree being a bit high? 


