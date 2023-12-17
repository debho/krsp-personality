library(ggplot2)
library(paletteer)
library(gtools)
library(gridExtra)
library(ggpubr)
library(visreg)
library(ggrepel)
library(LMERConvenienceFunctions)
library(patchwork)

# FIGURE 1 personality x density: autumn scatterplots ####
autumn_activity_scatter_fig <- visreg(survival_to_autumn,
                                      "oft1", by = "grid_density",
                                      gg = T, overlay = T,
                                      xlab = "Activity",
                                      ylab = "Probability of survival to autumn",
                                      point = list(alpha = 0.5,
                                                   size = 3),
                                      fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::colorblind_safe",
                          labels = c("Low Density",
                                     "Medium Density",
                                     "High Density")) +
  scale_fill_paletteer_d("ggprism::colorblind_safe",
                         labels = c("Low Density",
                                    "Medium Density",
                                    "High Density")) +
  labs(color = "Grid Density",
       fill = "Grid Density") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(15, 15, 0, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

autumn_activity_scatter_fig

autumn_aggression_scatter_fig <- visreg(survival_to_autumn,
                                        "mis1", by = "grid_density",
                                        gg = T, overlay = T,
                                        xlab = "Aggression",
                                        ylab = "Probability of survival to autumn",
                                        point = list(alpha = 0.5,
                                                     size = 3),
                                        fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::colorblind_safe",
                          labels = c("Low Density",
                                     "Medium Density",
                                     "High Density")) +
  scale_fill_paletteer_d("ggprism::colorblind_safe",
                         labels = c("Low Density",
                                    "Medium Density",
                                    "High Density")) +
  labs(color = "Grid Density",
       fill = "Grid Density") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(15, 15, 0, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

autumn_aggression_scatter_fig

# FIGURE 1 personality x density: autumn heatmaps ####
autumn_activity_heatmap <- visreg2d(survival_to_autumn,
                                    "oft1", "grid_density",
                                    plot.type = "gg",
                                    xlab = "Activity",
                                    ylab = "Grid Density") +
  scale_fill_paletteer_c("viridis::rocket") +
  labs(fill = "Probability of \nsurviving to autumn",
       tag = "A.") +
  theme_classic() +
  labs_pubr()+ 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.key.size = unit(45, "points"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(30, "point"),
        plot.margin = margin(15, 15, 0, 30), 
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

autumn_activity_heatmap

autumn_aggression_heatmap <- visreg2d(survival_to_autumn,
                                      "mis1", "grid_density",
                                      plot.type = "gg",
                                      xlab = "Aggression",
                                      ylab = "Grid Density") +
  scale_fill_paletteer_c("viridis::rocket") +
  labs(fill = "Probability of \nsurviving to autumn",
       tag = "B.") +
  theme_classic() +
  labs_pubr()+ 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.key.size = unit(45, "points"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(30, "point"),
        plot.margin = margin(15, 15, 0, 30), 
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

autumn_aggression_heatmap

# FIGURE 1 personality x density: overwinter scatterplots ####
overwinter_activity_scatter_fig <- visreg(survival_to_200d,
                                          "oft1", by = "grid_density",
                                          gg = T, overlay = T,
                                          xlab = "Activity",
                                          ylab = "Probability of overwinter survival",
                                          point = list(alpha = 0.5,
                                                       size = 3),
                                          fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::viridis",
                          labels = c("Low Density",
                                     "Medium Density",
                                     "High Density")) +
  scale_fill_paletteer_d("ggprism::viridis",
                         labels = c("Low Density",
                                    "Medium Density",
                                    "High Density")) +
  labs(color = "Grid Density",
       fill = "Grid Density") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(15, 15, 0, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

overwinter_activity_scatter_fig

overwinter_aggression_scatter_fig <- visreg(survival_to_200d,
                                            "mis1",by = "grid_density",
                                            gg = T, overlay = T,
                                            xlab = "Aggression",
                                            ylab = "Probability of overwinter survival",
                                            point = list(size = 3,
                                                         alpha = 0.5),
                                            fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::viridis",
                          labels = c("Low Density",
                                     "Medium Density",
                                     "High Density")) +
  scale_fill_paletteer_d("ggprism::viridis",
                         labels = c("Low Density",
                                    "Medium Density",
                                    "High Density")) +
  labs(color = "Grid Density",
       fill = "Grid Density") +
  theme_classic() +
  labs_pubr()+ 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(15, 15, 0, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

overwinter_aggression_scatter_fig

# FIGURE 1 personality x density: overwinter heatmaps ####
overwinter_activity_heatmap <- visreg2d(survival_to_200d,
                                           "oft1", "grid_density",
                                           plot.type = "gg",
                                           xlab = "Activity",
                                           ylab = "Grid Density") +
  scale_fill_paletteer_c("viridis::mako") +
  labs(fill = "Probability of \noverwinter survival",
       tag = "C.") +
  theme_classic() +
  labs_pubr()+ 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.key.size = unit(45, "points"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(30, "point"),
        plot.margin = margin(15, 15, 0, 30), 
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

overwinter_activity_heatmap

overwinter_aggression_heatmap <- visreg2d(survival_to_200d,
                                        "mis1", "grid_density",
                                        plot.type = "gg",
                                        xlab = "Aggression",
                                        ylab = "Grid Density") +
  scale_fill_paletteer_c("viridis::mako") +
  labs(fill = "Probability of \noverwinter survival",
       tag = "D.") +
  theme_classic() +
  labs_pubr()+ 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.key.size = unit(45, "points"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(30, "point"),
        plot.margin = margin(15, 15, 0, 30), 
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

overwinter_aggression_heatmap

# FIGURE 1 personality x density: complete ####
ggsave("figures/FINAL/fig 1_autumn.png",
       grid.arrange(autumn_activity_heatmap,
                    autumn_activity_scatter_fig,
                    autumn_aggression_heatmap,
                    autumn_aggression_scatter_fig,
                    ncol = 2,
                    nrow = 2))
ggsave("figures/FINAL/fig 1_overwinter.png",
       grid.arrange(overwinter_activity_heatmap,
                    overwinter_activity_scatter_fig,
                    overwinter_aggression_heatmap,
                    overwinter_aggression_scatter_fig,
                    ncol = 2,
                    nrow = 2))

# FIGURE 2 activity x aggression: scatterplots ####
autumn_personality_scatter_fig <- visreg(survival_to_autumn,
                                         "mis1", by = "oft1",
                                         gg = T, overlay = T,
                                         xlab = "Aggression",
                                         ylab = "Probability of survival to autumn",
                                         point = list(alpha = 0.5,
                                                      size = 3),
                                         fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::colorblind_safe",
                          labels = c("Low Activity",
                                     "Medium Activity",
                                     "High Activity")) +
  scale_fill_paletteer_d("ggprism::colorblind_safe",
                         labels = c("Low Activity",
                                    "Medium Activity",
                                    "High Activity")) + 
  labs(color = "Activity",
       fill = "Activity") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(15, 15, 0, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

autumn_personality_scatter_fig

overwinter_personality_scatter_fig <- visreg(survival_to_200d,
                                             "mis1", by = "oft1",
                                             gg = T, overlay = T,
                                             xlab = "Aggression",
                                             ylab = "Probability of overwinter survival",
                                             point = list(alpha = 0.5,
                                                          size = 3),
                                             fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::viridis",
                          labels = c("Low Activity",
                                     "Medium Activity",
                                     "High Activity")) +
  scale_fill_paletteer_d("ggprism::viridis",
                         labels = c("Low Activity",
                                    "Medium Activity",
                                    "High Activity")) +
  labs(color = "Activity",
       fill = "Activity") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(15, 15, 0, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

overwinter_personality_scatter_fig

# FIGURE 2 activity x aggression: heatmaps ####
autumn_personality_heatmap <- visreg2d(survival_to_autumn,
                                    "mis1", "oft1",
                                    plot.type = "gg",
                                    xlab = "Aggression",
                                    ylab = "Activity") +
  scale_fill_paletteer_c("viridis::rocket") +
  labs(fill = "Probability of \nsurviving to autumn",
       tag = "A.") +
  theme_classic() +
  labs_pubr()+ 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.key.size = unit(45, "points"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(30, "point"),
        plot.margin = margin(15, 15, 0, 30), 
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

autumn_personality_heatmap

overwinter_personality_heatmap <- visreg2d(survival_to_200d,
                                           "mis1", "oft1",
                                           plot.type = "gg",
                                           xlab = "Aggression",
                                           ylab = "Activity") +
  scale_fill_paletteer_c("viridis::mako") +
  labs(fill = "Probability of \noverwinter survival",
       tag = "B.") +
  theme_classic() +
  labs_pubr()+ 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.key.size = unit(45, "points"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(30, "point"),
        plot.margin = margin(15, 15, 0, 30), 
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1),
        plot.tag = element_text(size = 24,
                                face = "bold"))

overwinter_personality_heatmap

# FIGURE 2 activity x aggression: complete ####
ggsave("figures/fig 2.png",
       grid.arrange(autumn_personality_heatmap,
                    autumn_personality_scatter_fig,
                    overwinter_personality_heatmap,
                    overwinter_personality_scatter_fig,
                    ncol = 2,
                    nrow = 2))

# FIGURE S1: density by year ####
# making the dataframe for each gridyear combination
density_JOKLSU <- grids_density %>%
  filter(grid %in% c("JO", "KL", "SU"),
         !(grid == "JO" & year < 2015)) %>%
  group_by(grid, year) %>%
  mutate(treatment = factor(case_when((grid == "KL" |
                                         grid == "SU") ~
                                        "control",
                                      grid == "JO" ~
                                        "rattle")),
         gridyear = paste(grid, year))
# for positioning labels 
line_end <- density_JOKLSU %>%
  group_by(grid) %>%
  filter(year == 2022)
gridyears <- dat %>%
  select(grid,
         year) %>%
  mutate(gridyear = paste(grid, year)) %>%
  distinct() %>%
  left_join(density_JOKLSU,
            by = "gridyear") %>%
  select(gridyear,
         grid = grid.y,
         year = year.y,
         grid_density,
         treatment)

# making the graphs
density_gridyear <- ggplot(density_JOKLSU,
                           aes(year, grid_density, group = grid)) +
  geom_line(aes(col = treatment),
            linewidth = 0.75) +
  geom_point(data = gridyears,
             aes(year, grid_density, col = treatment),
             size = 6,
             alpha = 0.5,
             stroke = 1,
             show.legend = F) +
  geom_point(aes(col = treatment),
             size = 3) +
  geom_text_repel(data = line_end,
                  aes(label = grid,
                      col = treatment),
                  nudge_x = 0.75,
                  show.legend = F,
                  size = 5,
                  fontface = "bold") +
  geom_vline(data = gridyears,
             aes(xintercept = year),
             linetype = "dashed",
             size = 0.75,
             alpha = 0.5) +
  scale_color_paletteer_d("ggprism::colorblind_safe") +
  labs(x = "Year",
       y = "Spring Density (squirrels per ha)",
       col = "Treatment") + 
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.box.spacing = margin(0, -25, 0, 0),
        plot.margin = margin(30, 30, 30, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -2))

plot(density_gridyear)

# FIGURE S2: mastyear ####
dat_analysis <- dat %>%
  filter(!is.na(oft1),
         !is.na(mis1),
         !is.na(grid_density),
         !is.na(part_sc),
         !is.na(growth_sc))

survival_autumn_mast_fig <- ggplot(dat_analysis,
                                   aes(mastyear, made_it, fill = mastyear)) +
  theme_classic() +
  labs_pubr() +
  geom_jitter(aes(col = year),
              size = 3,
              show.legend = F) +
  scale_color_paletteer_d("ggthemes::colorblind") + 
  geom_violin(alpha = 0.6,
              show.legend = F) +
  scale_fill_paletteer_d("ggprism::colorblind_safe") +
  labs(x = "Mast Year?",
       y = "Probability of survival to autumn",
       col = "Year",
       fill = "Mast Year?",
       tag = "a.") + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.margin = margin(0, 30, 30, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -2)) 
  
plot(survival_autumn_mast_fig)

survival_200d_mast_fig <- ggplot(dat,
                                 aes(mastyear, survived_200d, fill = mastyear)) +
  theme_classic() +
  labs_pubr() +
  geom_jitter(aes(col = year),
              size = 3) +
  scale_color_paletteer_d("ggthemes::colorblind") + 
  geom_violin(alpha = 0.6) +
  scale_fill_paletteer_d("ggprism::colorblind_safe") +
  labs(x = "Mast Year?",
       y = "Probability of overwinter survival",
       col = "Year",
       fill = "Mast Year?",
       tag = "b.") + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.box.spacing = margin(0, -15, 0, 0),
        plot.margin = margin(0, 30, 30, 0),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -2))

plot(survival_200d_mast_fig)

ggsave("figures/survival~mast.png",
       grid.arrange(survival_autumn_mast_fig,
                    survival_200d_mast_fig,
                    ncol = 2,
                    nrow = 1))
