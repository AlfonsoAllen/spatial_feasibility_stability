
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
  mutate(alpha_ij = 1)
result_metrics_D_aux3 <- readr::read_csv("Results/result_metrics_D_aux3.csv") %>%
  mutate(alpha_ij = 1)
result_metrics_D_aux4 <- readr::read_csv("Results/result_metrics_D_aux4.csv") %>%
  mutate(alpha_ij = 1)


# PERFECT DIAGONAL DOMINANCE: alpha_ij = 0
result_metrics_D_aux2_PERFECT <- readr::read_csv("Results/result_metrics_PERFECT_D_aux2.csv") %>%
  mutate(alpha_ij = 0)
result_metrics_D_aux3_PERFECT <- readr::read_csv("Results/result_metrics_PERFECT_D_aux3.csv") %>%
  mutate(alpha_ij = 0)
result_metrics_D_aux4_PERFECT <- readr::read_csv("Results/result_metrics_PERFECT_D_aux4.csv") %>%
  mutate(alpha_ij = 0)

# HALF DIAGONAL DOMINANCE: alpha_ij = -0.5
result_metrics_D_aux2_HALF <- readr::read_csv("Results/result_metrics_HALF_D_aux2.csv") %>%
  mutate(alpha_ij = 0.5)
result_metrics_D_aux3_HALF <- readr::read_csv("Results/result_metrics_HALF_D_aux3.csv") %>%
  mutate(alpha_ij = 0.5)
result_metrics_D_aux4_HALF <- readr::read_csv("Results/result_metrics_HALF_D_aux4.csv") %>%
  mutate(alpha_ij = 0.5)



####################################################################
# MERGE ALL DATA

example_data <- dplyr::bind_rows(result_metrics_D_aux2,result_metrics_D_aux3,result_metrics_D_aux4,
                                 result_metrics_D_aux2_PERFECT,result_metrics_D_aux3_PERFECT,result_metrics_D_aux4_PERFECT,
                                 result_metrics_D_aux2_HALF,result_metrics_D_aux3_HALF,result_metrics_D_aux4_HALF)

# Modify the dataset to include LaTeX-compatible expressions
example_data$D_label <- (paste0("D[i]:", example_data$D))
example_data$alpha_label <- (paste0("-alpha[ij]:", example_data$alpha_ij))


ggplot(example_data, 
       aes(x = r1B, y = r2B, fill = perc_feasible_stable)) +
  geom_tile() +
  labs(x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill= "Feasible + stable points (%)") +
  scale_fill_gradientn(colours = wes_palette("Zissou1")) +
  facet_grid(rows = vars(D_label), cols = vars(alpha_label), 
             labeller = label_parsed, switch = "both") +  # Moves labels outside
  coord_fixed() +
  theme_bw()+
  theme(legend.position="bottom",
        legend.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))+
  guides(fill = guide_colorbar(title.theme = element_text(size = 18),,
                               barwidth = 20,  # Adjust this value to increase length
                               barheight = 1))  # Adjust height if needed))


png("Figures/Fig_Suppl_D3.png",
    width = 1.5*500*6.5, # The width of the plot in inches
    height = 1.5*520*4.4, res=300)

ggplot(example_data, 
       aes(x = r1B, y = r2B, fill = perc_feasible_stable)) +
  geom_tile() +
  labs(x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill= "Feasible + stable points (%)") +
  scale_fill_gradientn(colours = wes_palette("Zissou1")) +
  facet_grid(rows = vars(D_label), cols = vars(alpha_label), 
             labeller = label_parsed, switch = "both") +  # Moves labels outside
  coord_fixed() +
  theme_bw()+
  theme(legend.position="bottom",
        legend.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))+
  guides(fill = guide_colorbar(title.theme = element_text(size = 18),,
                               barwidth = 20,  # Adjust this value to increase length
                               barheight = 1))  # Adjust height if needed))

dev.off()


ggplot(example_data, 
       aes(x = r1B, y = r2B, fill = relmutinf)) +
  geom_tile() +
  labs(x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill= "Relative mutual information (U)") +
  scale_fill_gradientn(colours = wes_palette("Zissou1")) +
  facet_grid(rows = vars(D_label), cols = vars(alpha_label), 
             labeller = label_parsed, switch = "both") +  # Moves labels outside
  coord_fixed() +
  theme_bw()+
  theme(legend.position="bottom",
        legend.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))+
  guides(fill = guide_colorbar(title.theme = element_text(size = 18),,
                               barwidth = 20,  # Adjust this value to increase length
                               barheight = 1))  # Adjust height if needed))

png("Figures/Fig_Suppl_D4.png",
    width = 1.5*500*6.5, # The width of the plot in inches
    height = 1.5*520*4.4, res=300)

ggplot(example_data, 
       aes(x = r1B, y = r2B, fill = relmutinf)) +
  geom_tile() +
  labs(x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill= "Relative mutual information (U)") +
  scale_fill_gradientn(colours = wes_palette("Zissou1")) +
  facet_grid(rows = vars(D_label), cols = vars(alpha_label), 
             labeller = label_parsed, switch = "both") +  # Moves labels outside
  coord_fixed() +
  theme_bw()+
  theme(legend.position="bottom",
        legend.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))+
  guides(fill = guide_colorbar(title.theme = element_text(size = 18),,
                               barwidth = 20,  # Adjust this value to increase length
                               barheight = 1))  # Adjust height if needed))

dev.off()
