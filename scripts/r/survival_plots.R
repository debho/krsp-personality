library(ggplot2)
library(paletteer)
library(gtools)
library(gridExtra)
library(ggpubr)
library(mgcv)
library(visreg)
library(ggrepel)
library(LMERConvenienceFunctions)
library(patchwork)

# FIGURE 1 personality x density: scatterplots ####
autumn_activity <- gam(made_it ~
                         te(oft1, grid_density), data = dat)
autumn_aggression <- gam(made_it ~
                           te(mis1, grid_density), data = dat)
overwinter_activity <- gam(survived_200d ~
                             te(oft1, grid_density), data = dat)
overwinter_aggression <- gam(survived_200d ~
                               te(mis1, grid_density), data = dat)
vis.gam(autumn_activity,
        plot.type = "persp",
        color = "topo",
        theta = 225,
        phi = 5,
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Density",
        zlab = "Probability of survival to autumn") 
vis.gam(autumn_aggression,
        plot.type = "persp",
        color = "topo",
        theta = -135,
        phi = 15,
        ticktype = "detailed",
        xlab = "Aggression",
        ylab = "Density",
        zlab = "Probability of survival to autumn")

vis.gam(overwinter_activity,
        plot.type = "persp",
        color = "topo",
        theta = -135,
        phi = 15,
        ticktype = "detailed",
        xlab = "Activity",
        ylab = "Density",
        zlab = "Probability of survival overwinter")
vis.gam(overwinter_aggression,
        plot.type = "persp",
        color = "topo",
        theta = -135,
        phi = 15,
        ticktype = "detailed",
        xlab = "Aggression",
        ylab = "Density",
        zlab = "Probability of survival overwinter")

summary(autumn_activity)
summary(overwinter_activity)
summary(autumn_aggression)
summary(overwinter_aggression)

autumn_activity_scatter_fig <- visreg(survival_to_autumn,
                                      "grid_density", by = "oft1",
                                      gg = T, overlay = T,
                                      xlab = "Density",
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
       fill = "Activity",
       tag = "a.") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(0, 15, 15, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1))

autumn_activity_scatter_fig

autumn_aggression_scatter_fig <- visreg(survival_to_autumn,
                                        "grid_density", by = "mis1",
                                        gg = T, overlay = T,
                                        xlab = "Density",
                                        ylab = "Probability of survival to autumn",
                                        point = list(alpha = 0.5,
                                                     size = 3),
                                        fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::colorblind_safe",
                          labels = c("Low Aggression",
                                     "Medium Aggression",
                                     "High Aggression")) +
  scale_fill_paletteer_d("ggprism::colorblind_safe",
                         labels = c("Low Aggression",
                                    "Medium Aggression",
                                    "High Aggression")) +
  labs(color = "Aggression",
       fill = "Aggression",
       tag = "b.") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(0, 30, 15, 15),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1))

autumn_aggression_scatter_fig

ggsave("figures/survival~autumn.png",
       grid.arrange(autumn_activity_scatter_fig,
                    autumn_aggression_scatter_fig,
                    ncol = 2,
                    nrow = 1))

overwinter_activity_scatter_fig <- visreg(survival_to_200d,
                                          "grid_density", by = "oft1",
                                          gg = T, overlay = T,
                                          xlab = "Density",
                                          ylab = "Probability of survival overwinter",
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
       fill = "Activity",
       tag = "c.") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(15, 15, 0, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1))

overwinter_activity_scatter_fig

overwinter_aggression_scatter_fig <- visreg(survival_to_200d,
                                            "grid_density",by = "mis1",
                                            gg = T, overlay = T,
                                            xlab = "Density",
                                            ylab = "Probability of survival overwinter",
                                            point = list(size = 3,
                                                         alpha = 0.5),
                                            fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::viridis",
                          labels = c("Low Aggression",
                                     "Medium Aggression",
                                     "High Aggression")) +
  scale_fill_paletteer_d("ggprism::viridis",
                         labels = c("Low Aggression",
                                    "Medium Aggression",
                                    "High Aggression")) +
  labs(color = "Aggression",
       fill = "Aggression",
       tag = "d.") +
  theme_classic() +
  labs_pubr()+ 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(15, 30, 0, 15),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1))

overwinter_aggression_scatter_fig

ggsave("figures/survival~overwinter.png",
       grid.arrange(overwinter_activity_scatter_fig,
                    overwinter_aggression_scatter_fig,
                    ncol = 2,
                    nrow = 1))

# FIGURE 1 personality x density: 3-way plots ####
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
       fill = "Activity",
       tag = "a.") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(0, 15, 0, 30),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1))

autumn_personality_scatter_fig

overwinter_personality_scatter_fig <- visreg(survival_to_200d,
                                             "mis1", by = "oft1",
                                             gg = T, overlay = T,
                                             xlab = "Aggression",
                                             ylab = "Probability of survival overwinter",
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
       fill = "Activity",
       tag = "b.") +
  theme_classic() +
  labs_pubr() + 
  theme(axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(0, 30, 0, 15),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1))

overwinter_personality_scatter_fig

ggsave("figures/survival~personality.png",
       grid.arrange(autumn_personality_scatter_fig,
                    overwinter_personality_scatter_fig,
                    ncol = 2,
                    nrow = 1))

# FIGURE 2 activity x aggression: 3-way plots ####
autumn_personality_contour <- visreg2d(survival_to_autumn,
                                    "mis1", "oft1",
                                    plot.type = "gg",
                                    xlab = "Aggression",
                                    ylab = "Activity") +
  scale_fill_paletteer_c("viridis::rocket") +
  labs(fill = "Probability of \nsurviving to autumn") +
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
        axis.title.x = element_text(vjust = -1))

autumn_personality_contour

overwinter_personality_contour <- visreg2d(survival_to_200d,
                                           "mis1", "oft1",
                                           plot.type = "gg",
                                           xlab = "Aggression",
                                           ylab = "Activity") +
  scale_fill_paletteer_c("viridis::mako") +
  labs(fill = "Probability of \nsurviving overwinter") +
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
        plot.margin = margin(15, 30, 0, 15), 
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -1))

overwinter_personality_contour

ggsave("figures/survival~personalityCONTOUR.png",
       grid.arrange(autumn_personality_contour,
                    overwinter_personality_contour,
                    ncol = 2,
                    nrow = 1))
# FIGURE 2 activity x aggression: 3D plots ####
par(mfrow = c(1,2))

autumn_personality_3D <- plotLMER3d.fnc(survival_to_autumn,
                                        pred = "mis1",
                                        intr = "oft1",
                                        plot.type = "persp",
                                        color = "topo",
                                        xlab = "Aggression",
                                        ylab = "Activity",
                                        zlab = "Probability of survival to autumn",
                                        theta = -25,
                                        phi = 5)
overwinter_personality_3D <- plotLMER3d.fnc(survival_to_200d,
                                        pred = "mis1",
                                        intr = "oft1",
                                        plot.type = "persp",
                                        color = "topo",
                                        xlab = "Aggression",
                                        ylab = "Activity",
                                        zlab = "Probability of survival overwinter",
                                        theta = -25,
                                        phi = 5)

# FIGURE S1: density by year ####
# making the dataframe for each gridyear combination
density_JOKLSU <- grids_density %>%
  filter(grid %in% c("JO", "KL", "SU")) %>%
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
survival_autumn_mast_fig <- ggplot(dat,
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
        plot.margin = margin(30, 0, 30, 30),
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
        plot.margin = margin(30, 0, 30, 0),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -2))

plot(survival_200d_mast_fig)

ggsave("figures/survival~mast.png",
       grid.arrange(survival_autumn_mast_fig,
                    survival_200d_mast_fig,
                    ncol = 2,
                    nrow = 1))

# FIGURE ???: growth rate x density ####  
growth_autumn_fig <- visreg(survival_to_autumn,
                            "grid_density", by = "growth_sc",
                            gg = T, overlay = T,
                            xlab = "Density",
                            ylab = "Probability of survival to autumn",
                            point = list(alpha = 0.5),
                            fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::colorblind_safe",
                          labels = c("Low Growth Rate",
                                     "Medium Growth Rate",
                                     "High Growth Rate")) +
  scale_fill_paletteer_d("ggprism::colorblind_safe",
                         labels = c("Low Growth Rate",
                                    "Medium Growth Rate",
                                    "High Growth Rate")) +
  labs(color = "Growth Rate",
       fill = "Growth Rate") +
  theme_classic() +
  labs_pubr()

growth_autumn_fig

growth_overwinter_fig <- visreg(survival_to_200d,
                                            "grid_density",by = "growth_sc",
                                            gg = T, overlay = T,
                                            xlab = "Density",
                                            ylab = "Probability of survival overwinter",
                                            point = list(alpha = 0.5),
                                            fill = list(alpha = 0)) +
  scale_color_paletteer_d("ggprism::viridis",
                          labels = c("Low Growth Rate",
                                     "Medium Growth Rate",
                                     "High Growth Rate")) +
  scale_fill_paletteer_d("ggprism::viridis",
                         labels = c("Low Growth Rate",
                                    "Medium Growth Rate",
                                    "High Growth Rate")) +
  labs(color = "Growth Rate",
       fill = "Growth Rate") +
  theme_classic() +
  labs_pubr()

growth_overwinter_fig

autumn_growth <- gam(made_it ~
                       te(growth_sc, grid_density), data = dat)
overwinter_growth <- gam(survived_200d ~
                           te(growth_sc, grid_density), data = dat2)

vis.gam(autumn_growth,
        plot.type = "persp",
        color = "topo",
        theta = -135,
        phi = 15,
        ticktype = "detailed",
        xlab = "Growth Rate",
        ylab = "Density",
        zlab = "Probability of survival to autumn") 

vis.gam(overwinter_growth,
        plot.type = "persp",
        color = "topo",
        theta = -135,
        phi = 15,
        ticktype = "detailed",
        xlab = "Growth Rate",
        ylab = "Density",
        zlab = "Probability of survival overwinter") 

# FIGURE 2 ALT: personality x density ####
plotLMER3d.fnc(survival_to_autumn,
               pred = "oft1",
               intr = "grid_density",
               plot.type = "persp",
               color = "topo",
               xlab = "Activity",
               ylab = "Grid Density",
               zlab = "Probability of surviving to autumn",
               theta = -45,
               phi = 5)

plotLMER3d.fnc(survival_to_autumn,
               pred = "mis1",
               intr = "grid_density",
               plot.type = "persp",
               color = "topo",
               xlab = "Aggression",
               ylab = "Grid Density",
               zlab = "Probability of surviving to autumn",
               theta = -45,
               phi = 5)

plotLMER3d.fnc(survival_to_autumn,
               pred = "oft1",
               intr = "grid_density",
               plot.type = "persp",
               color = "topo",
               xlab = "Activity",
               ylab = "Grid Density",
               zlab = "Probability of surviving to autumn",
               theta = -45,
               phi = 5)

plotLMER3d.fnc(survival_to_200d,
               pred = "oft1",
               intr = "grid_density",
               plot.type = "persp",
               color = "topo",
               xlab = "Activity",
               ylab = "Grid Density",
               zlab = "Probability of surviving overwinter",
               theta = -45,
               phi = 5)

plotLMER3d.fnc(survival_to_200d,
               pred = "mis1",
               intr = "grid_density",
               plot.type = "persp",
               color = "topo",
               xlab = "Aggression",
               ylab = "Grid Density",
               zlab = "Probability of surviving overwinter",
               theta = -45,
               phi = 5)
