
library(tidyverse)
library(latex2exp)
library(wesanderson)
library(viridis)
library(patchwork)
####################################################################
# Load shape data

# Load shape data
# NEUTRAL COMPETITIVE SPECIES: alpha_ij = -1
result_metrics_D_aux2 <- readr::read_csv("Results/result_metrics_D_aux2.csv") %>%
  mutate(alpha_ij = -1)
result_metrics_D_aux3 <- readr::read_csv("Results/result_metrics_D_aux3.csv") %>%
  mutate(alpha_ij = -1)
result_metrics_D_aux4 <- readr::read_csv("Results/result_metrics_D_aux4.csv") %>%
  mutate(alpha_ij = -1)


# PERFECT DIAGONAL DOMINANCE: alpha_ij = 0
result_metrics_D_aux2_PERFECT <- readr::read_csv("Results/result_metrics_PERFECT_D_aux2.csv") %>%
  mutate(alpha_ij = 0)
result_metrics_D_aux3_PERFECT <- readr::read_csv("Results/result_metrics_PERFECT_D_aux3.csv") %>%
  mutate(alpha_ij = 0)
result_metrics_D_aux4_PERFECT <- readr::read_csv("Results/result_metrics_PERFECT_D_aux4.csv") %>%
  mutate(alpha_ij = 0)

# HALF DIAGONAL DOMINANCE: alpha_ij = -0.5
result_metrics_D_aux2_HALF <- readr::read_csv("Results/result_metrics_HALF_D_aux2.csv") %>%
  mutate(alpha_ij = -0.5)
result_metrics_D_aux3_HALF <- readr::read_csv("Results/result_metrics_HALF_D_aux3.csv") %>%
  mutate(alpha_ij = -0.5)
result_metrics_D_aux4_HALF <- readr::read_csv("Results/result_metrics_HALF_D_aux4.csv") %>%
  mutate(alpha_ij = -0.5)



####################################################################
# MERGE ALL DATA

example_data <- dplyr::bind_rows(result_metrics_D_aux2,result_metrics_D_aux3,result_metrics_D_aux4,
                                 result_metrics_D_aux2_PERFECT,result_metrics_D_aux3_PERFECT,result_metrics_D_aux4_PERFECT,
                                 result_metrics_D_aux2_HALF,result_metrics_D_aux3_HALF,result_metrics_D_aux4_HALF)





plot_metric_1 <- ggplot(example_data, aes(x = as.factor(D), 
                         y = perc_feasible_stable, 
                         fill = as.factor(alpha_ij))) +
  # geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +  # Violin plot
  # geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.5) +  # Boxplot for reference
  # stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black", position = position_dodge(0.9)) +  # Mean values
  geom_boxplot() +
  labs(y = "Percentage of growth vectors that result\nin feasible and stable populations (%)",
       x = "Dispersion rate (D)",
       fill = "Neutrality") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.text = element_text(size = 17),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17, face = "bold"),
        plot.title = element_text(size = 19, face = "bold"),
        strip.text = element_text(size = 17)) +
  guides(fill = guide_legend(title.theme = element_text(size = 17, face = "bold"))) +
  scale_fill_brewer(palette = "Set2")  # "Set2" is colorblind-friendly


plot_metric_2 <- ggplot(example_data,aes(x = as.factor(D), 
                        y = relmutinf, 
                        fill = as.factor(alpha_ij))) +
  geom_boxplot()+
  labs(y = "Relative mutual information",
       x = "Dispersion rate (D)",
       fill= "Neutrality")+
  theme_bw()+theme(legend.position="bottom")+
  theme(legend.position="bottom",
        legend.text = element_text(size = 17),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17, face = "bold"),
        plot.title = element_text(size = 19, face = "bold"),
        strip.text = element_text(size = 17)) +
  guides(fill = guide_legend(title.theme = element_text(size = 17, face = "bold"))) +
  scale_fill_brewer(palette = "Set2")


combined_plot <- (plot_metric_1 | plot_metric_2) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

png("Figures/Fig_2.png",
    width = 500*6.5, # The width of the plot in inches
    height = 520*3.5, res=300)

combined_plot

dev.off()
