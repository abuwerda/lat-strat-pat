library(tidyverse)
library(ggpubr)
library(plotly)
library(viridis)
library(RColorBrewer)
library(MASS)
library(dplyr)
library(ggplot2)

df <- read_csv("/Users/rosej/Library/CloudStorage/Dropbox/Documents/My Publications/2025 JAS/R/raw_data_v5.csv")

df_filtered <- df %>%
  dplyr::select(Site, Condensed, Art_Class, Art_Category,
                Condition, Patination, Patina_Condensed,
                Dissolution, Abrasion, Edge_Damage,
                Distance, Length, X_Coord, Y_Coord, Z_Coord)

# Color Palette
virdis_colors = viridis(7, option = "D", direction = -1)
color_mapping = setNames(virdis_colors, as.character(1:7))

# FIGURE 8 - 76 scatterplots

total_station_x <- 295
total_station_y <- 185
total_station_z <- 530.5
outcrop2_x <- 243
outcrop2_y <- 172
outcrop2_z <- 529.5

plot1 = ggplot(df %>% filter(Site==76),
               aes(x=X_Coord, y=Y_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Patina Stage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = outcrop2_x, y = outcrop2_y), color = "red", shape = 8, size = 4)

plot2 = ggplot(df %>% filter(Site==76),
               aes(x=X_Coord, y=Y_Coord, color=as.factor(Dissolution))) +
  geom_point()+
  labs(color="Dissolution") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = outcrop2_x, y = outcrop2_y), color = "red", shape = 8, size = 4)

plot3 = ggplot(df %>% filter(Site==76),
               aes(x=X_Coord, y=Y_Coord, color=as.factor(`Edge_Damage`))) +
  geom_point() +
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Edge Damage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = outcrop2_x, y = outcrop2_y), color = "red", shape = 8, size = 4)

plot4 = ggplot(df %>% filter(Site==76),
               aes(x=X_Coord, y=Y_Coord, color=as.factor(`Abrasion`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Abrasion") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4) +
  geom_point(aes(x = outcrop2_x, y = outcrop2_y), color = "red", shape = 8, size = 4)

plot5 = ggplot(df %>% filter(Site==76),
               aes(x=Y_Coord, y=Z_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "y-coord (m)",
       y = "z-coord (m)") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(legend.position="none") +
  geom_point(x = total_station_y, y = total_station_z, color = "black", shape = 8, size = 4) +
  geom_point(x = outcrop2_y, y = outcrop2_z, color = "red", shape = 8, size = 4) +
  expand_limits(y = 530.75) +
  coord_fixed(ratio = 1)

df_filtered <- df %>%
  filter(Site == 76, !is.na(Y_Coord), !is.na(Z_Coord))
kde <- kde2d(df_filtered$Y_Coord, df_filtered$Z_Coord, n = 100)
kde_df <- expand.grid(x = kde$x, y = kde$y)
kde_df$z <- as.vector(kde$z)
scale_factor <- 2
kde_df$y <- kde_df$y * scale_factor
kde_df$z[kde_df$z < 0.01] <- NA
total_station_z_exaggerated <- total_station_z * 2
outcrop2_z_exaggerated <- outcrop2_z * 2

plot6 = ggplot(data = kde_df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  scale_y_continuous(labels = NULL) +
  labs(x = "y-coord (m)", y = "z-coord", fill = "Density") +
  theme_classic() +
  coord_fixed() +
  expand_limits(y = 530.75 * 2) +
  geom_point(x = total_station_y, y = total_station_z_exaggerated, color = "black", shape = 8, size = 4) +
  geom_point(x = outcrop2_y, y = outcrop2_z_exaggerated, color = "red", shape = 8, size = 4)

plot = ggarrange(
  ggarrange(plot1, plot2, ncol = 2, nrow = 1),
  ggarrange(plot3, plot4, ncol = 2, nrow = 1),
  plot5,
  plot6,
  ncol = 1, nrow = 4,
  heights = c(1, 1, 1, .4)
)
plot = annotate_figure(plot,
                       bottom = text_grob("TH.76", size = 15, face = "bold"))
plot

# FIGURE 10 - 123 scatterplots

total_station_x <- 292.5
total_station_y <- 170
total_station_z <- 529.5

plot7 = ggplot(df %>% filter(Site==123),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color = "Patina Stage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic() +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot8 = ggplot(df %>% filter(Site==123),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Dissolution`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color= "Dissolution") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic() +
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot9 = ggplot(df %>% filter(Site==123),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Edge_Damage`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Edge Damage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot10 = ggplot(df %>% filter(Site==123),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Abrasion`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Abrasion") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot11 = ggplot(df %>% filter(Site==123),
                aes(x=X_Coord, y=Z_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "z-coord (m)") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(legend.position="none") +
  geom_point(aes(x = total_station_x, y = total_station_z), color = "black", shape = 8, size = 4) +
  expand_limits(y = 529.75) +
  coord_fixed(ratio = 1)


df_filtered <- df %>%
  filter(Site == 123, !is.na(Y_Coord), !is.na(Z_Coord))
kde <- kde2d(df_filtered$Y_Coord, df_filtered$Z_Coord, n = 100)
kde_df <- expand.grid(x = kde$x, y = kde$y)
kde_df$z <- as.vector(kde$z)
scale_factor <- 2
kde_df$y <- kde_df$y * scale_factor
kde_df$z[kde_df$z < 0.01] <- NA
total_station_z_exaggerated <- total_station_z * 2

plot12 = ggplot(data = kde_df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  scale_y_continuous(labels = NULL) +
  labs(x = "y-coord (m)", y = "z-coord", fill = "Density") +
  theme_classic() +
  coord_fixed() +
  expand_limits(y = 529.75 * 2) +
  expand_limits(x = 169) +
  geom_point(x = total_station_y, y = total_station_z_exaggerated, color = "black", shape = 8, size = 4)

plot = ggarrange(
  ggarrange(plot7, plot8, ncol = 2, nrow = 1),
  ggarrange(plot9, plot10, ncol = 2, nrow = 1),
  plot11,
  plot12,
  ncol = 1, nrow = 4,
  heights = c(1, 1, 1, .4)
)
plot = annotate_figure(plot,
                       bottom = text_grob("TH.123", size = 15, face = "bold"))
plot

# FIGURE 12 - 143 scatterplots

total_station_x <- 290
total_station_y <- 163.5
total_station_z <- 529

plot13 = ggplot(df %>% filter(Site==143),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color = "Patina Stage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic() +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot14 = ggplot(df %>% filter(Site==143),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Dissolution`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color= "Dissolution") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic() +
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot15 = ggplot(df %>% filter(Site==143),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Edge_Damage`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Edge Damage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot16 = ggplot(df %>% filter(Site==143),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Abrasion`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Abrasion") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot17 = ggplot(df %>% filter(Site==143),
                aes(x=X_Coord, y=Z_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "z-coord (m)") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(legend.position="none") +
  geom_point(aes(x = total_station_x, y = total_station_z), color = "black", shape = 8, size = 4) +
  expand_limits(y = 529.25) +
  coord_fixed(ratio = 1)

df_filtered <- df %>%
  filter(Site == 143, !is.na(X_Coord), !is.na(Z_Coord))
kde <- kde2d(df_filtered$X_Coord, df_filtered$Z_Coord, n = 100)
kde_df <- expand.grid(x = kde$x, y = kde$y)
kde_df$z <- as.vector(kde$z)
scale_factor <- 2
kde_df$y <- kde_df$y * scale_factor
kde_df$z[kde_df$z < 0.01] <- NA
total_station_z_exaggerated <- total_station_z * 2

plot18 = ggplot(data = kde_df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  scale_y_continuous(labels = NULL) +
  labs(x = "x-coord (m)", y = "z-coord", fill = "Density") +
  theme_classic() +
  coord_fixed() +
  expand_limits(y = 529.25 * 2) +
  geom_point(aes(x = total_station_x, y = total_station_z_exaggerated), color = "black", shape = 8, size = 4)

plot = ggarrange(
  ggarrange(plot13, plot14, ncol = 2, nrow = 1),
  ggarrange(plot15, plot16, ncol = 2, nrow = 1),
  plot17,
  plot18,
  ncol = 1, nrow = 4,
  heights = c(1, 1, 1, .4)
)
plot = annotate_figure(plot,
                       bottom = text_grob("TH.143", size = 15, face = "bold"))
plot

# FIGURE 14 - 187 scatterplots

total_station_x <- 293
total_station_y <- 165
total_station_z <- 529.5

plot19 = ggplot(df %>% filter(Site==187),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color = "Patina Stage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic() +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot20 = ggplot(df %>% filter(Site==187),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Dissolution`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color= "Dissolution") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot21 = ggplot(df %>% filter(Site==187),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Edge_Damage`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Edge Damage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot22 = ggplot(df %>% filter(Site==187),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Abrasion`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Abrasion") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot23 = ggplot(df %>% filter(Site==187),
                aes(x=Y_Coord, y=Z_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "y-coord (m)",
       y = "z-coord (m)") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(legend.position="none") +
  geom_point(aes(x = total_station_y, y = total_station_z), color = "black", shape = 8, size = 4) +
  expand_limits(y = 529.75) +
  coord_fixed(ratio = 1)

df_filtered <- df %>%
  filter(Site == 187, !is.na(Y_Coord), !is.na(Z_Coord))
kde <- kde2d(df_filtered$Y_Coord, df_filtered$Z_Coord, n = 100)
kde_df <- expand.grid(x = kde$x, y = kde$y)
kde_df$z <- as.vector(kde$z)
scale_factor <- 2
kde_df$y <- kde_df$y * scale_factor
kde_df$z[kde_df$z < 0.01] <- NA
total_station_z_exaggerated <- total_station_z * 2
plot24 = ggplot(data = kde_df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  scale_y_continuous(labels = NULL) +
  labs(x = "y-coord (m)", y = "z-coord", fill = "Density") +
  theme_classic() +
  coord_fixed() +
  expand_limits(y = 529.75 * 2) +
  geom_point(aes(x = total_station_y, y = total_station_z_exaggerated), color = "black", shape = 8, size = 4)

plot = ggarrange(
  ggarrange(plot19, plot20, ncol = 2, nrow = 1),
  ggarrange(plot21, plot22, ncol = 2, nrow = 1),
  plot23,
  plot24,
  ncol = 1, nrow = 4,
  heights = c(1, 1, 1, .4)
)
plot = annotate_figure(plot,
                       bottom = text_grob("TH.187", size = 15, face = "bold"))
plot

# FIGURE 16 - 188 scatterplots

total_station_x <- 287
total_station_y <- 190
total_station_z <- 532.5

plot25 = ggplot(df %>% filter(Site==188),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color = "Patina Stage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic() +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot26 = ggplot(df %>% filter(Site==188),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Dissolution`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color= "Dissolution") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot27 = ggplot(df %>% filter(Site==188),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Edge_Damage`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Edge Damage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot28 = ggplot(df %>% filter(Site==188),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Abrasion`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Abrasion") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot29 = ggplot(df %>% filter(Site==188),
                aes(x=Y_Coord, y=Z_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "y-coord (m)",
       y = "z-coord (m)") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  scale_x_reverse()+
  theme_classic()+
  theme(legend.position="none") +
  geom_point(aes(x = total_station_y, y = total_station_z), color = "black", shape = 8, size = 4) +
  expand_limits(y = 532.75) +
  coord_fixed(ratio = 1)

df_filtered <- df %>%
  filter(Site == 188, !is.na(Y_Coord), !is.na(Z_Coord))
kde <- kde2d(df_filtered$Y_Coord, df_filtered$Z_Coord, n = 100)
kde_df <- expand.grid(x = kde$x, y = kde$y)
kde_df$z <- as.vector(kde$z)
scale_factor <- 2
kde_df$y <- kde_df$y * scale_factor
kde_df$z[kde_df$z < 0.01] <- NA
total_station_z_exaggerated <- total_station_z * 2
plot30 = ggplot(data = kde_df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  scale_y_continuous(labels = NULL) +
  scale_x_reverse() +
  labs(x = "y-coord (m)", y = "z-coord", fill = "Density") +
  theme_classic() +
  coord_fixed() +
  expand_limits(y = 532.75 * 2) +
  geom_point(aes(x = total_station_y, y = total_station_z_exaggerated), color = "black", shape = 8, size = 4)

plot = ggarrange(
  ggarrange(plot25, plot26, ncol = 2, nrow = 1),
  ggarrange(plot27, plot28, ncol = 2, nrow = 1),
  plot29,
  plot30,
  ncol = 1, nrow = 4,
  heights = c(1, 1, 1, .6)
)
plot = annotate_figure(plot,
                       bottom = text_grob("TH.188", size = 15, face = "bold"))
plot

# FIGURE 18 - 419 scatterplots

total_station_x <- 287
total_station_y <- 161
total_station_z <- 529

plot31 = ggplot(df %>% filter(Site==419 & Patination!=99),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color = "Patina Stage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic() +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot32 = ggplot(df %>% filter(Site==419),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Dissolution`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color= "Dissolution") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot33 = ggplot(df %>% filter(Site==419),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Edge_Damage`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Edge Damage") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot34 = ggplot(df %>% filter(Site==419),
                aes(x=X_Coord, y=Y_Coord, color=as.factor(`Abrasion`))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "y-coord (m)",
       color="Abrasion") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(axis.title=element_blank()) +
  geom_point(aes(x = total_station_x, y = total_station_y), color = "black", shape = 8, size = 4)

plot35 = ggplot(df %>% filter(Site==419),
                aes(x=X_Coord, y=Z_Coord, color=as.factor(Patination))) +
  geom_point()+
  labs(x = "x-coord (m)",
       y = "z-coord (m)") +
  scale_color_viridis(discrete = TRUE, option = "D", direction=-1) +
  theme_classic()+
  theme(legend.position="none") +
  geom_point(aes(x = total_station_x, y = total_station_z), color = "black", shape = 8, size = 4) +
  expand_limits(y = 529.25) +
  coord_fixed(ratio = 1)

df_filtered <- df %>%
  filter(Site == 419, !is.na(X_Coord), !is.na(Z_Coord))
kde <- kde2d(df_filtered$X_Coord, df_filtered$Z_Coord, n = 100)
kde_df <- expand.grid(x = kde$x, y = kde$y)
kde_df$z <- as.vector(kde$z)
scale_factor <- 2
kde_df$y <- kde_df$y * scale_factor
kde_df$z[kde_df$z < 0.01] <- NA
total_station_z_exaggerated <- total_station_z * 2
plot36 = ggplot(data = kde_df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  scale_y_continuous(labels = NULL) +
  labs(x = "x-coord (m)", y = "z-coord", fill = "Density") +
  theme_classic() +
  coord_fixed() +
  expand_limits(y = 529.25 * 2) +
  geom_point(aes(x = total_station_x, y = total_station_z_exaggerated), color = "black", shape = 8, size = 4)

plot = ggarrange(
  ggarrange(plot31, plot32, ncol = 2, nrow = 1),
  ggarrange(plot33, plot34, ncol = 2, nrow = 1),
  plot35,
  plot36,
  ncol = 1, nrow = 4,
  heights = c(1, 1, .4)
)
plot = annotate_figure(plot,
                       bottom = text_grob("TH.419", size = 15, face = "bold"))
plot

# FIGURE 7 - Distance Boxplots: Debitage - Patina

plot37 = ggplot(df %>% filter(Art_Class == "Debitage"),
                aes(x = as.factor(Patination), y = Distance,
                    fill = as.factor(Patination))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "",
       y = "distance (m)",
       fill="Patina Stage",
       title="Debitage") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot37

df_76 = df %>% filter(Site == 76 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Patination), data = df_76)

df_123 = df %>% filter(Site == 123 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Patination), data = df_123)
pairwise.wilcox.test(df_123$Distance, as.factor(df_123$Patination), p.adjust.method = "BH")

df_143 = df %>% filter(Site == 143 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Patination), data = df_143)
pairwise.wilcox.test(df_143$Distance, as.factor(df_143$Patination), p.adjust.method = "BH")

df_187 = df %>% filter(Site == 187 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Patination), data = df_187)
pairwise.wilcox.test(df_187$Distance, as.factor(df_187$Patination), p.adjust.method = "BH")

df_188 = df %>% filter(Site == 188 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Patination), data = df_188)
pairwise.wilcox.test(df_188$Distance, as.factor(df_188$Patination), p.adjust.method = "BH")

df_419 = df %>% filter(Site == 419 & Art_Class == "Debitage" & Patination!=99)
kruskal.test(Distance ~ as.factor(Patination), data = df_419)
pairwise.wilcox.test(df_419$Distance, as.factor(df_419$Patination), p.adjust.method = "BH")

# FIGURE S2 - Distance Boxplots: Tools - Patina

plot38 = ggplot(df %>% filter(Patination!=99 & Art_Class == "Tool"),
                aes(x = as.factor(Patination), y = Distance,
                    fill = as.factor(Patination))) +

  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  labs(title = "Tools",
       x = "",
       y = "distance (m)",
       fill="Patina Stage") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot38

df_76 = df %>% filter(Site == 76 & Art_Class == "Tool" & Patination!=99)
kruskal.test(Distance ~ as.factor(Patination), data = df_76)

df_123 = df %>% filter(Site == 123 & Art_Class == "Tool" & Patination!=99)
kruskal.test(Distance ~ as.factor(Patination), data = df_123)

df_143 = df %>% filter(Site == 143 & Art_Class == "Tool" & Patination!=99)
kruskal.test(Distance ~ as.factor(Patination), data = df_143)

df_187 = df %>% filter(Site == 187 & Art_Class == "Tool" & Patination!=99)
kruskal.test(Distance ~ as.factor(Patination), data = df_187)

df_188 = df %>% filter(Site == 188 & Art_Class == "Tool" & Patination!=99)
kruskal.test(Distance ~ as.factor(Patination), data = df_188)

df_419 = df %>% filter(Site == 419 & Art_Class == "Tool" & Patination!=99)
kruskal.test(Distance ~ as.factor(Patination), data = df_419)
pairwise.wilcox.test(df_419$Distance, as.factor(df_419$Patination), p.adjust.method = "BH")

# FIGURE S1 - Distance Boxplots: Core - Patina

plot39 = ggplot(df %>% filter(Art_Class == "Core" & Patination!=99),
                aes(x = as.factor(Patination), y = Distance,
                    fill = as.factor(Patination))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  labs(fill = "Patina Stage",
       x = "",
       y = "distance (m)",
       title="Cores") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot39

df_76 = df %>% filter(Site == 76 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Patination), data = df_76)

df_123 = df %>% filter(Site == 123 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Patination), data = df_123)

df_143 = df %>% filter(Site == 143 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Patination), data = df_143)

df_187 = df %>% filter(Site == 187 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Patination), data = df_187)
pairwise.wilcox.test(df_187$Distance, as.factor(df_187$Patination), p.adjust.method = "BH")

df_188 = df %>% filter(Site == 188 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Patination), data = df_188)

df_419 = df %>% filter(Patination!= 99 & Site == 419 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Patination), data = df_419)
pairwise.wilcox.test(df_419$Distance, as.factor(df_419$Patination), p.adjust.method = "BH")

# FIGURE S3 - Distance Boxplots: Debitage - Dissolution

plot40 = ggplot(df %>% filter(Art_Class == "Debitage"),
                aes(x = as.factor(Dissolution), y = Distance,
                    fill = as.factor(Dissolution))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(fill = "Dissolution",
       x = "",
       y = "distance (m)",
       title="Debitage") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot40

df_76 = df %>% filter(Site == 76 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_76)
pairwise.wilcox.test(df_76$Distance, as.factor(df_76$Dissolution), p.adjust.method = "BH")

df_123 = df %>% filter(Site == 123 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_123)
pairwise.wilcox.test(df_123$Distance, as.factor(df_123$Dissolution), p.adjust.method = "BH")

df_143 = df %>% filter(Site == 143 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_143)
pairwise.wilcox.test(df_143$Distance, as.factor(df_143$Dissolution), p.adjust.method = "BH")

df_187 = df %>% filter(Site == 187 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_187)
pairwise.wilcox.test(df_187$Distance, as.factor(df_187$Dissolution), p.adjust.method = "BH")

df_188 = df %>% filter(Site == 188 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_188)
pairwise.wilcox.test(df_188$Distance, as.factor(df_188$Dissolution), p.adjust.method = "BH")

df_419 = df %>% filter(Site == 419 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_419)
pairwise.wilcox.test(df_419$Distance, as.factor(df_419$Dissolution), p.adjust.method = "BH")

# FIGURE S5 - Distance Boxplots: Tools - Dissolution

plot41 = ggplot(df %>% filter(Art_Class == "Tool"),
                aes(x = as.factor(Dissolution), y = Distance,
                    fill = as.factor(Dissolution))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  labs(fill = "Dissolution",
       x = "",
       y = "distance",
       title="Tools") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot41

df_76 = df %>% filter(Site == 76 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_76)

df_123 = df %>% filter(Site == 123 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_123)

df_143 = df %>% filter(Site == 143 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_143)

df_187 = df %>% filter(Site == 187 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_187)

df_188 = df %>% filter(Site == 188 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_188)

df_419 = df %>% filter(Site == 419 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_419)

# FIGURE S4 - Distance Boxplots: Cores - Dissolution

plot42 = ggplot(df %>% filter(Patination !=99 & Art_Class == "Core"),
                aes(x = as.factor(Dissolution), y = Distance,
                    fill = as.factor(Dissolution))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  labs(fill = "Dissolution",
       x = "",
       y = "distance (m)",
       title="Cores") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot42

df_76 = df %>% filter(Site == 76 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_76)

df_123 = df %>% filter(Site == 123 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_123)
pairwise.wilcox.test(df_123$Distance, as.factor(df_123$Dissolution), p.adjust.method = "BH")

df_143 = df %>% filter(Site == 143 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_143)

df_187 = df %>% filter(Site == 187 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_187)
pairwise.wilcox.test(df_187$Distance, as.factor(df_187$Dissolution), p.adjust.method = "BH")

df_188 = df %>% filter(Site == 188 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_188)

df_419 = df %>% filter(Site == 419 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Dissolution), data = df_419)
pairwise.wilcox.test(df_419$Distance, as.factor(df_419$Dissolution), p.adjust.method = "BH")

# FIGURE S6- Distance Boxplots: Debitage - Edge Damage

plot43 = ggplot(df %>% filter(Art_Class == "Debitage"),
                aes(x = as.factor(Edge_Damage), y = Distance,
                    fill = as.factor(Edge_Damage))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(fill = "Edge Damage",
       x = "",
       y = "distance (m)",
       title="Debitage") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot43

df_76 = df %>% filter(Site == 76 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_76)
pairwise.wilcox.test(df_76$Distance, as.factor(df_76$Edge_Damage), p.adjust.method = "BH")

df_123 = df %>% filter(Site == 123 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_123)
pairwise.wilcox.test(df_123$Distance, as.factor(df_123$Edge_Damage), p.adjust.method = "BH")

df_143 = df %>% filter(Site == 143 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_143)
pairwise.wilcox.test(df_143$Distance, as.factor(df_143$Edge_Damage), p.adjust.method = "BH")

df_187 = df %>% filter(Site == 187 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_187)
pairwise.wilcox.test(df_187$Distance, as.factor(df_187$Edge_Damage), p.adjust.method = "BH")

df_188 = df %>% filter(Site == 188 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_188)
pairwise.wilcox.test(df_188$Distance, as.factor(df_188$Edge_Damage), p.adjust.method = "BH")

df_419 = df %>% filter(Site == 419 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_419)
pairwise.wilcox.test(df_419$Distance, as.factor(df_419$Edge_Damage), p.adjust.method = "BH")

# FIGURE S8 - Distance Boxplots: Tools - Edge Damage

plot44 = ggplot(df %>% filter(Art_Class == "Tool"),
                aes(x = as.factor(Edge_Damage), y = Distance,
                    fill = as.factor(Edge_Damage))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  labs(fill = "Edge Damage",
       x = "",
       y = "distance (m)",
       title="Tools") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot44

df_76 = df %>% filter(Site == 76 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_76)

df_123 = df %>% filter(Site == 123 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_123)

df_143 = df %>% filter(Site == 143 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_143)
pairwise.wilcox.test(df_143$Distance, as.factor(df_143$Edge_Damage), p.adjust.method = "BH")

df_187 = df %>% filter(Site == 187 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_187)

df_188 = df %>% filter(Site == 188 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_188)

df_419 = df %>% filter(Site == 419 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_419)

# FIGURE S7 - Distance Boxpots: Cores - Edge Damage

plot45 = ggplot(df %>% filter(Art_Class == "Core"),
                aes(x = as.factor(Edge_Damage), y = Distance,
                    fill = as.factor(Edge_Damage))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  labs(fill = "Edge Damage",
       x = "",
       y = "distance (m)",
       title="Cores") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot45

df_76 = df %>% filter(Site == 76 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_76)

df_123 = df %>% filter(Site == 123 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_123)

df_143 = df %>% filter(Site == 143 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_143)

df_187 = df %>% filter(Site == 187 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_187)

df_188 = df %>% filter(Site == 188 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_188)
pairwise.wilcox.test(df_188$Distance, as.factor(df_188$Edge_Damage), p.adjust.method = "BH")

df_419 = df %>% filter(Site == 419 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Edge_Damage), data = df_419)
pairwise.wilcox.test(df_419$Distance, as.factor(df_419$Edge_Damage), p.adjust.method = "BH")

# FIGURE S9 - Distance Boxplots: Debitage - Abrasion

plot46 = ggplot(df %>% filter(Art_Class == "Debitage"),
                aes(x = as.factor(Abrasion), y = Distance,
                    fill = as.factor(Abrasion))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(fill = "Abrasion",
       x = "",
       y = "distance (m)",
       title="Debitage") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot46

df_76 = df %>% filter(Site == 76 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_76)
pairwise.wilcox.test(df_76$Distance, as.factor(df_76$Abrasion), p.adjust.method = "BH")

df_123 = df %>% filter(Site == 123 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_123)
pairwise.wilcox.test(df_123$Distance, as.factor(df_123$Abrasion), p.adjust.method = "BH")

df_143 = df %>% filter(Site == 143 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_143)
pairwise.wilcox.test(df_143$Distance, as.factor(df_143$Abrasion), p.adjust.method = "BH")

df_187 = df %>% filter(Site == 187 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_187)
pairwise.wilcox.test(df_187$Distance, as.factor(df_187$Abrasion), p.adjust.method = "BH")

df_188 = df %>% filter(Site == 188 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_188)

df_419 = df %>% filter(Site == 419 & Art_Class == "Debitage")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_419)
pairwise.wilcox.test(df_419$Distance, as.factor(df_419$Abrasion), p.adjust.method = "BH")

# FIGURE S11 - Distance Boxplots: Tools - Abrasion

plot47 = ggplot(df %>% filter(Art_Class == "Tool"),
                aes(x = as.factor(Abrasion), y = Distance,
                    fill = as.factor(Abrasion))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  labs(fill = "Abrasion",
       x = "",
       y = "distance (m)",
       title="Tools") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot47

df_76 = df %>% filter(Site == 76 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_76)

df_123 = df %>% filter(Site == 123 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_123)

df_143 = df %>% filter(Site == 143 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_143)

df_187 = df %>% filter(Site == 187 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_187)

df_188 = df %>% filter(Site == 188 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_188)

df_419 = df %>% filter(Site == 419 & Art_Class == "Tool")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_419)

# FIGURE S10 - Distance Boxplots: Cores - Abrasion

plot48 = ggplot(df %>% filter(Art_Class == "Core"),
                aes(x = as.factor(Abrasion), y = Distance,
                    fill = as.factor(Abrasion))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  labs(fill = "Abrasion",
       x = "",
       y = "distance (m)",
       title="Cores") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction=-1)+
  theme_classic() +
  theme(plot.title=element_text(hjust=.5))+
  facet_wrap(~as.factor(Site))
plot48

df_76 = df %>% filter(Site == 76 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_76)

df_123 = df %>% filter(Site == 123 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_123)

df_143 = df %>% filter(Site == 143 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_143)

df_187 = df %>% filter(Site == 187 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_187)
pairwise.wilcox.test(df_187$Distance, as.factor(df_187$Abrasion), p.adjust.method = "BH")

df_188 = df %>% filter(Site == 188 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_188)

df_419 = df %>% filter(Site == 419 & Art_Class == "Core")
kruskal.test(Distance ~ as.factor(Abrasion), data = df_419)
pairwise.wilcox.test(df_419$Distance, as.factor(df_419$Abrasion), p.adjust.method = "BH")

# FIGURE 20 - Technology Histograms: Patination

custom_labels=c(
  bif_tools="Bifacial Tools",
  blades_cores="Blade Cores & Products",
  flakes_cores="Flake Cores & Products",
  Levallois="Levallois Cores & Products"
)

ggplot(data = df %>% filter(Patination!=99 & !is.na(Art_Category))) +
  geom_histogram(aes(y = ..count.., x = Patination, fill = as.factor(Patination)),
                 color = "black", binwidth = 1) +
  geom_density(aes(y = ..count.., x = Patination), color = 2) +
  scale_fill_manual(values = color_mapping) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1,7, by = 1)) +
  labs(fill = "Patina Stage", y = "artifact count", x = "") +
  facet_wrap(~Art_Category, scales = "free_y", labeller=as_labeller(custom_labels))

# FIGURE S13 - Technology Histograms: Dissolution

ggplot(data = df %>% filter(!is.na(Art_Category))) +
  geom_histogram(aes(y = ..count.., x = Dissolution, fill = as.factor(Dissolution)),
                 color = "black", binwidth = 1) +
  geom_density(aes(y = ..count.., x = Dissolution), color = 2) +
  scale_fill_manual(values = color_mapping) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1,7, by = 1)) +
  labs(fill = "Dissolution", y = "artifact count", x = "") +
  facet_wrap(~Art_Category, scales = "free_y", labeller=as_labeller(custom_labels))

# FIGURE S14 - Technology Histograms: Edge Damage

ggplot(data = df %>% filter(!is.na(Art_Category))) +
  geom_histogram(aes(y = ..count.., x = Edge_Damage, fill = as.factor(Edge_Damage)),
                 color = "black", binwidth = 1) +
  geom_density(aes(y = ..count.., x = Edge_Damage), color = 2) +
  scale_fill_manual(values = color_mapping) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1,7, by = 1)) +
  labs(fill = "Edge Damage", y = "artifact count", x = "") +
  facet_wrap(~Art_Category, scales = "free_y", labeller=as_labeller(custom_labels))

# FIGURE S15 - Technology Histograms: Abrasion

ggplot(data = df %>% filter(!is.na(Art_Category))) +
  geom_histogram(aes(y = ..count.., x = Abrasion, fill = as.factor(Abrasion)),
                 color = "black", binwidth = 1) +
  geom_density(aes(y = ..count.., x = Abrasion), color = 2) +
  scale_fill_manual(values = color_mapping) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1,7, by = 1)) +
  labs(fill = "Abrasion", y = "artifact count", x = "") +
  facet_wrap(~Art_Category, scales = "free_y", labeller=as_labeller(custom_labels))

# FIGURE 22 - Nubian Core Metric Box Plots: Patination

plot49 = ggplot(df %>% filter(Patination!=99 & Patination!=1 & Patination!=2 & Condensed == "Lev_Nubian"),
                aes(x = as.factor(Patination), y = Length,
                    fill = as.factor(Patination))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Patina Stage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot49

df_Nubian = df %>% filter(Patination!=99 & Patination!=1 & Patination!=2 & Condensed == "Lev_Nubian")
kruskal.test(Length ~ as.factor(Patination), data = df_Nubian)
pairwise.wilcox.test(df_Nubian$Length, as.factor(df_Nubian$Patination), p.adjust.method = "BH")

# FIGURE S16 - Nubian Core Metric Box Plots: Dissolution

plot50 = ggplot(df %>% filter(Condensed == "Lev_Nubian" & Dissolution!=6),
                aes(x = as.factor(Dissolution), y = Length,
                    fill = as.factor(Dissolution))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "",
       y = "length (mm)",
       fill="Dissolution") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot50

df_Nubian = df %>% filter(Condensed == "Lev_Nubian" & Dissolution!=6)
kruskal.test(Length ~ as.factor(Dissolution), data = df_Nubian)
pairwise.wilcox.test(df_Nubian$Length, as.factor(df_Nubian$Dissolution), p.adjust.method = "BH")

# FIGURE S17 - Nubian Core Metric Box Plots: Edge Damage

plot51 = ggplot(df %>% filter(Condensed == "Lev_Nubian" & Edge_Damage!=5),
                aes(x = as.factor(Edge_Damage), y = Length,
                    fill = as.factor(Edge_Damage))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "",
       y = "length (mm)",
       fill="Edge Damage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot51

df_Nubian = df %>% filter(Condensed == "Lev_Nubian")
kruskal.test(Length ~ as.factor(Edge_Damage), data = df_Nubian)

# FIGURE S18 - Nubian Core Metric Box Plots: Abrasion

plot52 = ggplot(df %>% filter(Condensed == "Lev_Nubian"),
                aes(x = as.factor(Abrasion), y = Length,
                    fill = as.factor(Abrasion))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "",
       y = "length (mm)",
       fill="Abrasion") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot52

df_Nubian = df %>% filter(Condensed == "Lev_Nubian")
kruskal.test(Length ~ as.factor(Abrasion), data = df_Nubian)
pairwise.wilcox.test(df_Nubian$Length, as.factor(df_Nubian$Abrasion), p.adjust.method = "BH")

# FIGURE 21  - Blades & Blade Cores Metric Boxplots: Patination
plot53 = ggplot(df %>% filter(Patination!=99 & Art_Category == "blades_cores" & Condition == "c"),
                aes(x = as.factor(Patination), y = Length,
                    fill = as.factor(Patination))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Patina Stage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot53

df_bladesandcores = df %>% filter(Patination!=99 & Art_Category == "blades_cores" & Condition == "c")
kruskal.test(Length ~ as.factor(Patination), data = df_bladesandcores)
pairwise.wilcox.test(df_bladesandcores$Length, as.factor(df_bladesandcores$Patination), p.adjust.method = "BH")

# FIGURE S19  - Blades & Blade Cores Metric Boxplots: Dissolution

plot54 = ggplot(df %>% filter(Art_Category == "blades_cores" & Condition == "c"),
                aes(x = as.factor(Dissolution), y = Length,
                    fill = as.factor(Dissolution))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Dissolution") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot54

df_bladesandcores = df %>% filter(Art_Category == "blades_cores" & Condition == "c")
kruskal.test(Length ~ as.factor(Dissolution), data = df_bladesandcores)
pairwise.wilcox.test(df_bladesandcores$Length, as.factor(df_bladesandcores$Dissolution), p.adjust.method = "BH")

# FIGURE S20  - Blades & Blade Cores Metric Boxplots: Edge Damage

plot55 = ggplot(df %>% filter(Art_Category == "blades_cores" & Condition == "c"),
                aes(x = as.factor(Edge_Damage), y = Length,
                    fill = as.factor(Edge_Damage))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Edge Damage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot55

df_bladesandcores = df %>% filter(Art_Category == "blades_cores" & Condition == "c")
kruskal.test(Length ~ as.factor(Edge_Damage), data = df_bladesandcores)
pairwise.wilcox.test(df_bladesandcores$Length, as.factor(df_bladesandcores$Edge_Damage), p.adjust.method = "BH")

# FIGURE S21  - Blades & Blade Cores Metric Boxplots: Abrasion

plot56 = ggplot(df %>% filter(Art_Category == "blades_cores" & Condition == "c"),
                aes(x = as.factor(Abrasion), y = Length,
                    fill = as.factor(Abrasion))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Abrasion") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot56

df_bladesandcores = df %>% filter(Art_Category == "blades_cores" & Condition == "c")
kruskal.test(Length ~ as.factor(Abrasion), data = df_bladesandcores)
pairwise.wilcox.test(df_bladesandcores$Length, as.factor(df_bladesandcores$Abrasion), p.adjust.method = "BH")

# FIGURE S12 - 419 Nubian Core lengths: patina groups condensed

plot57 = ggplot(df %>% filter(Patination!=99 & Condensed == "Lev_Nubian" & Site == "419"),
                aes(x = as.factor(Patina_Condensed), y = Length,
                    fill = as.factor(Patina_Condensed))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1) +
  labs(x = "", y = "length (mm)",
       fill="Patina Stage") +
  scale_fill_manual(values = color_mapping) +
  theme_classic()
plot57

df_419Nubian = df %>% filter(Patination!=99 & Site== "419" & Condensed == "Lev_Nubian")
kruskal.test(Length ~ as.factor(Patina_Condensed), data = df_419Nubian)
pairwise.wilcox.test(df_419Nubian$Length, as.factor(df_419Nubian$Patina_Condensed), p.adjust.method = "BH")