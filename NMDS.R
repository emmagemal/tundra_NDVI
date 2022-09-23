### Library ----
library(tidyverse)
library(vegan)

### Data ---- 
matrix.sp <- read.csv("Data/nmds_matrix.csv", header = T)


### NMDS ###
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

# saving as a cheat-sheet 
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
orditorp(c.nmds, display = "species",col = "red", air = 0.01)

## Plotting NMDS
# extracting NMDS scores (x and y coordinates)
data.scores.c <- as.data.frame(scores(c.nmds))
data.scores.c$site <- plots.c$site
data.scores.c$elevation <- plots.c$elevation

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
                    geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, group = group,
                                                    color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = site, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_color_manual(values = c("#698B22", "#CD8500"),
                                       labels = c("Katterjokk", "Nissonjokk"),
                                       name = "Site") +
                    scale_shape_discrete(labels = c("Katterjokk", "Nissonjokk"),
                                         name = "Site") +
                    scale_fill_manual(values = c("#698B22", "#CD8500"),
                                      labels = c("Katterjokk", "Nissonjokk"),
                                      name = "Site"))

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

(nmds_plot.c2 <- ggplot(data.scores.c, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.c2, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = elevation, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_shape_discrete(labels = c("Katterjokk", "Nissonjokk"),
                                         name = "Site"))


## SHORT SHRUBS ----  
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

(nmds_plot.s1 <- ggplot(data.scores.s, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.s1, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = site, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_color_manual(values = c("#698B22", "#CD8500"),
                                       labels = c("Katterjokk", "Nissonjokk"),
                                       name = "Site") +
                    scale_shape_discrete(labels = c("Katterjokk", "Nissonjokk"),
                                         name = "Site") +
                    scale_fill_manual(values = c("#698B22", "#CD8500"),
                                      labels = c("Katterjokk", "Nissonjokk"),
                                      name = "Site"))

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

(nmds_plot.s2 <- ggplot(data.scores.s, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.s2, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = elevation, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_shape_discrete(labels = c("Katterjokk", "Nissonjokk"),
                                         name = "Site"))


## TALL SHRUBS --
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

ordiplot(t.nmds)
orditorp(t.nmds, display = "species", col = "red", air = 0.01)

# plot
ordiplot(t.nmds, type = "n")
ordiplot(t.nmds$points)
ordihull(t.nmds,groups = plots.c$site, draw = "polygon", label = F, col = colvec)


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

(nmds_plot.t1 <- ggplot(data.scores.t, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.t1, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = site, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_color_manual(values = c("#698B22", "#CD8500"),
                                       labels = c("Katterjokk", "Nissonjokk"),
                                       name = "Site") +
                    scale_shape_discrete(labels = c("Katterjokk", "Nissonjokk"),
                                         name = "Site") +
                    scale_fill_manual(values = c("#698B22", "#CD8500"),
                                      labels = c("Katterjokk", "Nissonjokk"),
                                      name = "Site"))

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

(nmds_plot.t2 <- ggplot(data.scores.t, aes(x = NMDS1, y = NMDS2)) + 
                    geom_polygon(data = df_ell.t2, aes(x = NMDS1, y = NMDS2, group = group,
                                                       color = group, fill = group), alpha = 0.2, 
                                 size = 0.5, linetype = 1) +
                    geom_point(aes(color = elevation, pch = site), size = 2) +
                    theme_bw() + 
                    theme(legend.position = "right") + 
                    labs(x = "NMDS1", y = "NMDS2") +
                    scale_shape_discrete(labels = c("Katterjokk", "Nissonjokk"),
                                         name = "Site"))


## Q for Emil: is it ok to plot an NMDS with > 2 dimensions in 2D? (axis 1 ~ axis 2)
  # am I losing too much info/ how would I pick what axes to show? 
  # if I can get it to converge with 2 dimensions, is this worth chosing over more despite the
    # scree being a bit high? 


