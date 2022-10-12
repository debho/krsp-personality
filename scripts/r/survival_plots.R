library(ggplot2)
library(paletteer)
library(gtools)

survival_residuals <- dat %>%
  filter(!is.na(growth_sc)) %>%
  mutate(survival_to_autumn_res = scale(predict(survival_to_autumn)),
         survival_to_200d_res = scale(predict(survival_to_200d)))

survival_autumn_oft_fig <- ggplot(survival_residuals,
                                  aes(oft1, made_it, col = grid_density)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = grid_density),
              method = "lm",
              se = F) +
  scale_color_paletteer_c("viridis::viridis")
plot(survival_autumn_oft_fig)

survival_autumn_mis_fig <- ggplot(survival_residuals,
                                  aes(mis1, made_it, col = grid_density)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = grid_density),
              method = "lm",
              se = F) +
  scale_color_paletteer_c("viridis::viridis")
plot(survival_autumn_mis_fig)

survival_200d_oft_fig <- ggplot(survival_residuals,
                                  aes(oft1, survived_200d, col = grid_density)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = grid_density),
              method = "lm",
              se = F) +
  scale_color_paletteer_c("viridis::plasma")
plot(survival_200d_oft_fig)

survival_200d_mis_fig <- ggplot(survival_residuals,
                                  aes(mis1, survived_200d, col = grid_density)) +
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = grid_density),
              method = "lm",
              se = F) +
  scale_color_paletteer_c("viridis::plasma")
plot(survival_200d_mis_fig)

