# library(shiny)
# library(shinythemes)
# library(tidyverse)
# library(plotly)
# library(stringr)
# library(cluster)
# library(FactoMineR)
# library(devtools)
# library(factoextra)
# library(reshape2)
# library(ggthemes)
# library(NbClust)
# library(readxl)
# library(RColorBrewer)
# library(data.table)
# library(maps)

# Data Cleaning

# hpi <- read_xlsx("hpi-data-2016.xlsx", sheet = 5, col_names = TRUE)
# hpi <- hpi[-c(1:4, 145:161), ]
# names(hpi) <- c("Rank", "Country", "Region", "Avg.Life.Expectancy", 
#                 "Avg.Wellbeing", "Happy.Life.Years", "Footprint.gha", 
#                 "Inequality", "Inequality.LE", "Inequality.W", 
#                 "HPI", "GDP", "Population", "GINI.Index")
# hpi$Rank <- as.integer(hpi$Rank)
# hpi$Region <- as.factor(hpi$Region)
# hpi$Avg.Life.Expectancy <- as.numeric(hpi$Avg.Life.Expectancy)
# hpi$Avg.Wellbeing <- as.numeric(hpi$Avg.Wellbeing)
# hpi$Happy.Life.Years <- as.numeric(hpi$Happy.Life.Years)
# hpi$Footprint.gha <- as.numeric(hpi$Footprint.gha)
# hpi$Inequality <- as.numeric(hpi$Inequality)
# hpi$Inequality.LE <- as.numeric(hpi$Inequality.LE)
# hpi$Inequality.W <- as.numeric(hpi$Inequality.W)
# hpi$HPI <- as.numeric(hpi$HPI)
# hpi$GDP <- as.numeric(hpi$GDP)
# hpi$Population <- as.numeric(hpi$Population)
# saveRDS(hpi, "hpi.rds")

hpi <- readRDS("hpi.rds")

# var_hpi <- c(
#   "Life Expectancy",
#   "Wellbeing",
#   "Inequality",
#   "Ecological Footprint",
#   "Happy Planet Index"
# )

# EDA
hpi_map <- hpi[, c("Country", "Region", "Avg.Life.Expectancy", 
                   "Avg.Wellbeing", "Footprint.gha", "Inequality", 
                   "HPI", "GDP", "Population")]
map <- map_data("world")
map$region[map$region == "USA"] <- "United States of America"
map$region[map$region == "UK"] <- "United Kingdom"
hpi_map <- left_join(map, hpi_map, by = c("region" = "Country"))

# HPImap <- ggplotly(
#   ggplot(hpi_map, aes(x = long, 
#                       y = lat, 
#                       group = group, 
#                       fill = HPI,
#                       colour = HPI,
#                       text = paste("Country : ", hpi_map$region, "\n",
#                                    "HPI : ", round(hpi_map$HPI, 2), "\n",
#                                    sep = ""))
#   ) + 
#     geom_polygon() +
#     scale_fill_gradient2(low = "red3", 
#                          high = "green4", 
#                          mid = "yellow2", 
#                          midpoint = 30) +
#     scale_colour_gradient2(low = "red3", 
#                            high = "green4", 
#                            mid = "yellow2", 
#                            midpoint = 30) +
#     coord_equal() +
#     theme_solid() +
#     ggtitle("Happy Planet Index") +
#     labs(x = NULL,
#          y = NULL),
#   tooltip = "text")
# saveRDS(HPImap, "HPImap.rds")

HPImap <- readRDS("HPImap.rds")

# LEmap <- ggplotly(
#   ggplot(hpi_map, aes(x = long, 
#                       y = lat, 
#                       group = group, 
#                       fill = Avg.Life.Expectancy,
#                       colour = Avg.Life.Expectancy,
#                       text = paste("Country : ", hpi_map$region, "\n",
#                                    "Life Expectancy : ", floor(hpi_map$Avg.Life.Expectancy), " years", "\n",
#                                    sep = ""))
#   ) + 
#     geom_polygon() +
#     scale_fill_gradient2(low = "red3", 
#                          high = "green4", 
#                          mid = "yellow2", 
#                          midpoint = 65) +
#     scale_colour_gradient2(low = "red3", 
#                            high = "green4", 
#                            mid = "yellow2", 
#                            midpoint = 65) +
#     coord_equal() +
#     theme_solid() +
#     ggtitle("Happy Planet Index") +
#     labs(x = NULL,
#          y = NULL),
#   tooltip = "text")
# saveRDS(LEmap, "LEmap.rds")

LEmap <- readRDS("LEmap.rds")

# WBmap <- ggplotly(
#   ggplot(hpi_map, aes(x = long, 
#                       y = lat, 
#                       group = group, 
#                       fill = Avg.Wellbeing,
#                       colour = Avg.Wellbeing,
#                       text = paste("Country : ", hpi_map$region, "\n",
#                                    "Wellbeing : ", round(hpi_map$Avg.Wellbeing, 2), "\n",
#                                    sep = ""))
#   ) + 
#     geom_polygon() +
#     scale_fill_gradient2(low = "red3", 
#                          high = "green4", 
#                          mid = "yellow2", 
#                          midpoint = 5.25) +
#     scale_colour_gradient2(low = "red3", 
#                            high = "green4", 
#                            mid = "yellow2", 
#                            midpoint = 5.25) +
#     coord_equal() +
#     theme_solid() +
#     ggtitle("Happy Planet Index") +
#     labs(x = NULL,
#          y = NULL),
#   tooltip ="text")
# saveRDS(WBmap, "WBmap.rds")

WBmap <- readRDS("WBmap.rds")

# Imap <- ggplotly(
#   ggplot(hpi_map, aes(x = long, 
#                       y = lat, 
#                       group = group, 
#                       fill = Inequality,
#                       colour = Inequality,
#                       text = paste("Country : ", hpi_map$region, "\n",
#                                    "Inequality :", round(hpi_map$Inequality * 100, 2), "%", "\n",
#                                    sep = ""))
#   ) + 
#     geom_polygon() +
#     scale_fill_gradient2(high = "red3", 
#                          low = "green4", 
#                          mid = "yellow2", 
#                          midpoint = 0.25) +
#     scale_colour_gradient2(high = "red3", 
#                            low = "green4", 
#                            mid = "yellow2", 
#                            midpoint = 0.25) +
#     coord_equal() +
#     theme_solid() +
#     ggtitle("Happy Planet Index") +
#     labs(x = NULL,
#          y = NULL),
#   tooltip = "text")
# saveRDS(Imap, "Imap.rds")

Imap <- readRDS("Imap.rds")

# Fmap <- ggplotly(
#   ggplot(hpi_map, aes(x = long, 
#                       y = lat, 
#                       group = group, 
#                       fill = Footprint.gha,
#                       colour = Footprint.gha,
#                       text = paste("Country : ", hpi_map$region, "\n",
#                                    "Footprint : ", round(hpi_map$Footprint.gha, 2), " gha", "\n",
#                                    sep = ""))
#   ) + 
#     geom_polygon() +
#     scale_fill_gradient2(high = "red3", 
#                          low = "green4", 
#                          mid = "yellow2", 
#                          midpoint = 8) +
#     scale_colour_gradient2(high = "red3", 
#                            low = "green4", 
#                            mid = "yellow2", 
#                            midpoint = 8) +                 
#     coord_equal() +
#     theme_solid() +
#     ggtitle("Happy Planet Index") +
#     labs(x = NULL,
#          y = NULL),
#   tooltip ="text")
# saveRDS(Fmap, "Fmap.rds")

Fmap <- readRDS("Fmap.rds")


# Unsupervised Learning

hpi_scale <- scale(hpi[, 4:13])
# heatmap_HPI <-
#   plot_ly(
#     x = colnames(hpi_scale), y = colnames(hpi_scale),
#     z = round(cor(hpi_scale), 3),
#     colors = "Spectral",
#     type="heatmap") %>%
#   layout(title = "Heatmap of HPI Variable Correlation Matrix")
# saveRDS(heatmap_HPI, "heatmap_HPI.rds")
 
heatmap_HPI <- readRDS("heatmap_HPI.rds")


  # qplot(x=Var1, y=Var2, data=melt(cor(hpi_scale)), fill = value, geom="tile") +
  #                    scale_fill_gradient2(limits=c(-1,1)) +
  # theme_classic() +
  # theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  # labs(title = "Heatmap of Correlation Matrix",
  #      x=NULL,
  #      y=NULL)
  


# Principal Component Analysis
hpi.pca <- PCA(hpi_scale, graph=FALSE)
PCA_viz <- fviz_pca_var(hpi.pca, col.var="contrib",
                        gradient.cols = c("red", "yellow", "blue"),
                        repel = TRUE) +
  labs(title = "Variables - Principal Component Analysis",
       x = "Principal Component 1 (66.7%)",
       y = "Principal Component 2 (13.1%)")
# number <- NbClust(hpi_scale, 
#                   distance="euclidean",
#                   min.nc=2, 
#                   max.nc=15, # By default, max.nc=15
#                   method='ward.D', 
#                   index='all', 
#                   alphaBeale = 0.1)
set.seed(2018)
pam <- pam(hpi_scale, diss=FALSE, k = 3, keep.data=TRUE)
Cluster_viz <- fviz_cluster(pam, stand = FALSE, geom = "point",
                            ellipse.type = "norm", ggtheme = theme_classic()) +
  labs(title = "Clustering Plot",
       x = "Principal Component 1 (66.7%)",
       y = "Principal Component 2 (13.1%)")

hpi['Cluster'] <- as.factor(pam$clustering)
# cluster_map <- left_join(map, hpi[, c("Region", "Country", "Avg.Life.Expectancy", 
#                                       "Avg.Wellbeing", "Footprint.gha", "Inequality", 
#                                       "HPI", "GDP", "Population", "Cluster")], 
#                          by = c("region" = "Country"))
# cluster_map$Cluster <- as.character(cluster_map$Cluster)
# cluster_map[is.na(cluster_map$Cluster),"Cluster"] <- "Not Available"
# cluster_map$Cluster <- as.factor(cluster_map$Cluster)
# saveRDS(cluster_map, "cluster_map.rds")

cluster_map <- readRDS("cluster_map.rds")

# cluster_map_v <-
#   ggplotly(
#     ggplot(cluster_map, aes(x = long, 
#                             y = lat, 
#                             group = group, 
#                             fill = Cluster,
#                             colour = Cluster,
#                             text = paste("Country : ", cluster_map$region, "\n",
#                                          "Life Exp : ", floor(cluster_map$Avg.Life.Expectancy), " years", "\n",
#                                          "Wellbeing : ", round(cluster_map$Avg.Wellbeing, 2), "\n",
#                                          "Inequality : ", round(cluster_map$Inequality*100, 2), " %", "\n",
#                                          "Footprint : ", round(cluster_map$Footprint.gha, 2), " gha", "\n",
#                                          "GDP : USD ", floor(cluster_map$GDP), "\n",
#                                          "Population : ", format(cluster_map$Population, big.mark = ","), "\n",
#                                          "HPI : ", round(cluster_map$HPI, 2), "\n",
#                                          sep = ""))
#     ) + 
#       geom_polygon() +
#       scale_fill_manual(values = c("red3", "goldenrod1", "seagreen3", "gray77")) +
#       scale_colour_manual(values = c("red3", "goldenrod1", "seagreen3", "gray77")) +
#       coord_equal() +
#       theme_solid() +
#       ggtitle("Clustering Happy Planet Index") +
#       labs(x = NULL,
#            y = NULL),
#     tooltip ="text")
# saveRDS(cluster_map_v, "cluster_map_v.rds")

cluster_map_v <- readRDS("cluster_map_v.rds")

# cluster_map_1 <-
#   ggplotly(
#     ggplot(cluster_map, aes(x = long, 
#                             y = lat, 
#                             group = group, 
#                             fill = Cluster,
#                             colour = Cluster,
#                             text = paste("Country : ", cluster_map$region, "\n",
#                                          "Life Exp : ", floor(cluster_map$Avg.Life.Expectancy), " years", "\n",
#                                          "Wellbeing : ", round(cluster_map$Avg.Wellbeing, 2), "\n",
#                                          "Inequality : ", round(cluster_map$Inequality*100, 2), " %", "\n",
#                                          "Footprint : ", round(cluster_map$Footprint.gha, 2), " gha", "\n",
#                                          "GDP : USD ", floor(cluster_map$GDP), "\n",
#                                          "Population : ", format(cluster_map$Population, big.mark = ","), "\n",
#                                          "HPI : ", round(cluster_map$HPI, 2), "\n",
#                                          sep = ""))
#     ) + 
#       geom_polygon() +
#       scale_fill_manual(values = c("red3", "gray48", "gray48", "gray77")) +
#       scale_colour_manual(values = c("red3", "gray48", "gray48", "gray77")) +
#       coord_equal() +
#       theme_solid() +
#       ggtitle("Clustering Happy Planet Index") +
#       labs(x = NULL,
#            y = NULL),
#     tooltip ="text")
# saveRDS(cluster_map_1, "cluster_map_1.rds")

cluster_map_1 <- readRDS("cluster_map_1.rds")

# cluster_map_2 <-
#   ggplotly(
#     ggplot(cluster_map, aes(x = long, 
#                             y = lat, 
#                             group = group, 
#                             fill = Cluster,
#                             colour = Cluster,
#                             text = paste("Country : ", cluster_map$region, "\n",
#                                          "Life Exp : ", floor(cluster_map$Avg.Life.Expectancy), " years", "\n",
#                                          "Wellbeing : ", round(cluster_map$Avg.Wellbeing, 2), "\n",
#                                          "Inequality : ", round(cluster_map$Inequality*100, 2), " %", "\n",
#                                          "Footprint : ", round(cluster_map$Footprint.gha, 2), " gha", "\n",
#                                          "GDP : USD ", floor(cluster_map$GDP), "\n",
#                                          "Population : ", format(cluster_map$Population, big.mark = ","), "\n",
#                                          "HPI : ", round(cluster_map$HPI, 2), "\n",
#                                          sep = ""))
#     ) + 
#       geom_polygon() +
#       scale_fill_manual(values = c("gray48", "goldenrod1", "gray48", "gray77")) +
#       scale_colour_manual(values = c("gray48", "goldenrod1", "gray48", "gray77")) +
#       coord_equal() +
#       theme_solid() +
#       ggtitle("Clustering Happy Planet Index") +
#       labs(x = NULL,
#            y = NULL),
#     tooltip ="text")
# saveRDS(cluster_map_2, "cluster_map_2.rds")

cluster_map_2 <- readRDS("cluster_map_2.rds")

# cluster_map_3 <-
#   ggplotly(
#     ggplot(cluster_map, aes(x = long, 
#                             y = lat, 
#                             group = group, 
#                             fill = Cluster,
#                             colour = Cluster,
#                             text = paste("Country : ", cluster_map$region, "\n",
#                                          "Life Exp : ", floor(cluster_map$Avg.Life.Expectancy), " years", "\n",
#                                          "Wellbeing : ", round(cluster_map$Avg.Wellbeing, 2), "\n",
#                                          "Inequality : ", round(cluster_map$Inequality*100, 2), " %", "\n",
#                                          "Footprint : ", round(cluster_map$Footprint.gha, 2), " gha", "\n",
#                                          "GDP : USD ", floor(cluster_map$GDP), "\n",
#                                          "Population : ", format(cluster_map$Population, big.mark = ","), "\n",
#                                          "HPI : ", round(cluster_map$HPI, 2), "\n",
#                                          sep = ""))
#     ) + 
#       geom_polygon() +
#       scale_fill_manual(values = c("gray48", "gray48", "seagreen3", "gray77")) +
#       scale_colour_manual(values = c("gray48", "gray48", "seagreen3", "gray77")) +
#       coord_equal() +
#       theme_solid() +
#       ggtitle("Clustering Happy Planet Index") +
#       labs(x = NULL,
#            y = NULL),
#     tooltip ="text")
# saveRDS(cluster_map_3, "cluster_map_3.rds")

cluster_map_3 <- readRDS("cluster_map_3.rds")

# Insight

ClustText <- paste(
  "Cluster 1 are mostly from Sub Saharan Africa, the countries experiencing conflicts 
                  such as Afghanistan, Syria, and Myanmar. With low income (average GDP is USD 1917), 
                  low wellbeing score (average 4.34) and low life expectancy (60 years old), the average 
                  HPI of the countries in this cluster is 20.11, the lowest from the three.",
  "Cluster 2 are dominated with post-communist and developing countries in Asia Pacific and Latin Americas.
                  The average HPI of the countries in this cluster is 29.29, increases significantly compared to cluster 
                  1's average HPI. The average life expectancy is 73 years and mean ecological footprint is 3.02 gha, 
                  but the average inequality is still in 19.83%.",
  "Cluster 3, the happiest of them all. Dominated by developed countries and surprisingly, Chile and Costa Rica, which scores the highest with 44.71.
                  The average HPI of the countries in this cluster is 29.03, almost the same with cluster 2, but there's a big difference in average 
                  life expectancy (73.48 vs 80.80), average wellbeing (5.404 vs 6.897), inequality (19.83% vs 9.31%), and carbon footprint (3.022 gha vs 6.114 gha). 
                  The countries in cluster 3 produce more carbon footprint than countries in cluster 2 (more than twice per capita) 
                  and that what makes the HPI score is practically the same with countries in cluster 2.", sep = "\n")

Clust1Text <- paste(
"Cluster 1 are mostly from Sub Saharan Africa, the countries experiencing conflicts 
        such as Afghanistan, Syria, and Myanmar. With low income (average GDP is USD 1917), 
        low wellbeing score (average 4.34) and low life expectancy (60 years old), 
        the average HPI of the countries in this cluster is 20.11, the lowest from the three.")

Clust2Text <- paste(
"Cluster 2 are dominated with post-communist and developing countries in Asia Pacific and Latin Americas.
        The average HPI of the countries in this cluster is 29.29, increases significantly compared to cluster 
        1's average HPI, but almost the same with cluster 3 (29.03). There's a big difference in average life expectancy 
        (73.48 vs 80.80), average wellbeing (5.404 vs 6.897), inequality (19.83% vs 9.31%), and ecological footprint (3.022 gha vs 6.114 gha). 
        The countries in cluster 3 produce more ecological footprint than countries in this cluster (more than twice per capita) 
        and that what makes the HPI score is practically the same with countries in cluster 3.")

Clust3Text <- paste(
"Cluster 3, the happiest of them all. Dominated by developed countries and surprisingly, Chile and Costa Rica, which scores the highest with 44.71.
                  The average HPI of the countries in this cluster is 29.03, almost the same with cluster 2, but there's a big difference in average 
                  life expectancy (73.48 vs 80.80), average wellbeing (5.404 vs 6.897), inequality (19.83% vs 9.31%), and carbon footprint (3.022 gha vs 6.114 gha). 
                  The countries in this cluster produce more carbon footprint than countries in cluster 2 (more than twice per capita) 
                  and that what makes the HPI score is practically the same with countries in cluster 2.", sep = "")

# HPI_table <- hpi[, c("Country", "HPI")]
# HPI_table <- HPI_table[order(HPI_table$HPI, decreasing = TRUE), ]
# HPI_table$Ranking <- c(1:140)
# HPI_table$HPI <- round(HPI_table$HPI, 2)
# col_order <- c("Ranking", "Country", "HPI")
# HPI_table <- HPI_table[c(1:5, 136:140), col_order]
# 
# saveRDS(HPI_table, "HPI_table.rds")

HPI_table <- readRDS("HPI_table.rds")


# LE_table <- hpi[, c("Country", "Avg.Life.Expectancy")]
# LE_table <- LE_table[order(LE_table$Avg.Life.Expectancy, decreasing = TRUE), ]
# LE_table$Ranking <- c(1:140)
# LE_table$Avg.Life.Expectancy <- floor(LE_table$Avg.Life.Expectancy)
# col_order <- c("Ranking", "Country", "Avg.Life.Expectancy")
# LE_table <- LE_table[c(1:5, 136:140), col_order]
# names(LE_table) <- c("Ranking", "Country", "Life Expectancy (years)")
# 
# saveRDS(LE_table, "LE_table.rds")

LE_table <- readRDS("LE_table.rds")


# WB_table <- hpi[, c("Country", "Avg.Wellbeing")]
# WB_table <- WB_table[order(WB_table$Avg.Wellbeing, decreasing = TRUE), ]
# WB_table$Ranking <- c(1:140)
# WB_table$Avg.Wellbeing <- round(WB_table$Avg.Wellbeing, 2)
# col_order <- c("Ranking", "Country", "Avg.Wellbeing")
# WB_table <- WB_table[c(1:5, 136:140), col_order]
# names(WB_table) <- c("Ranking", "Country", "Wellbeing")
# 
# saveRDS(WB_table, "WB_table.rds")

WB_table <- readRDS("WB_table.rds")

# I_table <- hpi[, c("Country", "Inequality")]
# I_table <- I_table[order(I_table$Inequality, decreasing = FALSE), ]
# I_table$Ranking <- c(1:140)
# col_order <- c("Ranking", "Country", "Inequality")
# I_table <- I_table[c(1:5, 136:140), col_order]
# I_table$Inequality <- round(I_table$Inequality * 100, 2)
# names(I_table) <- c("Ranking", "Country", "Inequality (%)")
# 
# saveRDS(I_table, "I_table.rds")

I_table <- readRDS("I_table.rds")

# F_table <- hpi[, c("Country", "Footprint.gha")]
# F_table <- F_table[order(F_table$Footprint.gha, decreasing = FALSE), ]
# F_table$Ranking <- c(1:140)
# F_table$Footprint.gha <- round(F_table$Footprint.gha, 2)
# col_order <- c("Ranking", "Country", "Footprint.gha")
# F_table <- F_table[c(1:5, 136:140), col_order]
# names(F_table) <- c("Ranking", "Country", "Footprint (gha)")
# 
# saveRDS(F_table, "F_table.rds")

F_table <- readRDS("F_table.rds")


