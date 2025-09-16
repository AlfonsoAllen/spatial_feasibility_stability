library(ggplot2)
library(dplyr)
library(readr)
library(latex2exp)
library(patchwork)
####################################################################
# Create grid for r1 and r2, X and Y
r1B_aux = round(-0.1,3)
r2B_aux = round(0.055,3)

####################################################################
# Load data
# NEUTRAL COMPETITIVE SPECIES: alpha_ij = -1
data_D_aux2 <- readr::read_csv("Results/data_FD_example_r1B_minus0p1_r2B0p055_100xD.csv") %>%
  mutate(alpha_ij = 1)
data_D_aux3 <- readr::read_csv("Results/data_FD_example_r1B_minus0p1_r2B0p055_10xD.csv") %>%
  mutate(alpha_ij = 1)
data_D_aux4 <- readr::read_csv("Results/data_FD_example_r1B_minus0p1_r2B0p055_1xD.csv") %>%
  mutate(alpha_ij = 1)

data_D_aux2$type[data_D_aux2$type == "Feasible"] <- "Feasible and not stable"
data_D_aux2$type[data_D_aux2$type == "Feasible and Stable"] <- "Feasible and stable"
data_D_aux2$type[data_D_aux2$type == "Not feasible"] <- "Exclusion"
data_D_aux3$type[data_D_aux3$type == "Feasible"] <- "Feasible and not stable"
data_D_aux3$type[data_D_aux3$type == "Feasible and Stable"] <- "Feasible and stable"
data_D_aux3$type[data_D_aux3$type == "Not feasible"] <- "Exclusion"
data_D_aux4$type[data_D_aux4$type == "Feasible"] <- "Feasible and not stable"
data_D_aux4$type[data_D_aux4$type == "Feasible and Stable"] <- "Feasible and stable"
data_D_aux4$type[data_D_aux4$type == "Not feasible"] <- "Exclusion"


####################################################################
# Load data
# PERFECT DIAGONAL DOMINANCE: alpha_ij = 0
data_D_aux2_PERFECT <- readr::read_csv("Results/data_FD_example_PERFECT_r1B_minus0p1_r2B0p055_100xD.csv") %>%
  mutate(alpha_ij = 0)
data_D_aux3_PERFECT <- readr::read_csv("Results/data_FD_example_PERFECT_r1B_minus0p1_r2B0p055_10xD.csv") %>%
  mutate(alpha_ij = 0)
data_D_aux4_PERFECT <- readr::read_csv("Results/data_FD_example_PERFECT_r1B_minus0p1_r2B0p055_1xD.csv") %>%
  mutate(alpha_ij = 0)

data_D_aux2_PERFECT$type[data_D_aux2_PERFECT$type == "Feasible"] <- "Feasible and not stable"
data_D_aux2_PERFECT$type[data_D_aux2_PERFECT$type == "Feasible and Stable"] <- "Feasible and stable"
data_D_aux2_PERFECT$type[data_D_aux2_PERFECT$type == "Not feasible"] <- "Exclusion"
data_D_aux3_PERFECT$type[data_D_aux3_PERFECT$type == "Feasible"] <- "Feasible and not stable"
data_D_aux3_PERFECT$type[data_D_aux3_PERFECT$type == "Feasible and Stable"] <- "Feasible and stable"
data_D_aux3_PERFECT$type[data_D_aux3_PERFECT$type == "Not feasible"] <- "Exclusion"
data_D_aux4_PERFECT$type[data_D_aux4_PERFECT$type == "Feasible"] <- "Feasible and not stable"
data_D_aux4_PERFECT$type[data_D_aux4_PERFECT$type == "Feasible and Stable"] <- "Feasible and stable"
data_D_aux4_PERFECT$type[data_D_aux4_PERFECT$type == "Not feasible"] <- "Exclusion"

####################################################################
# Load data
# HALF DIAGONAL DOMINANCE: alpha_ij = -0.5
data_D_aux2_HALF <- readr::read_csv("Results/data_FD_example_HALF_r1B_minus0p1_r2B0p055_100xD.csv") %>%
  mutate(alpha_ij = 0.5)
data_D_aux3_HALF <- readr::read_csv("Results/data_FD_example_HALF_r1B_minus0p1_r2B0p055_10xD.csv") %>%
  mutate(alpha_ij = 0.5)
data_D_aux4_HALF <- readr::read_csv("Results/data_FD_example_HALF_r1B_minus0p1_r2B0p055_1xD.csv") %>%
  mutate(alpha_ij = 0.5)

data_D_aux2_HALF$type[data_D_aux2_HALF$type == "Feasible"] <- "Feasible and not stable"
data_D_aux2_HALF$type[data_D_aux2_HALF$type == "Feasible and Stable"] <- "Feasible and stable"
data_D_aux2_HALF$type[data_D_aux2_HALF$type == "Not feasible"] <- "Exclusion"
data_D_aux3_HALF$type[data_D_aux3_HALF$type == "Feasible"] <- "Feasible and not stable"
data_D_aux3_HALF$type[data_D_aux3_HALF$type == "Feasible and Stable"] <- "Feasible and stable"
data_D_aux3_HALF$type[data_D_aux3_HALF$type == "Not feasible"] <- "Exclusion"
data_D_aux4_HALF$type[data_D_aux4_HALF$type == "Feasible"] <- "Feasible and not stable"
data_D_aux4_HALF$type[data_D_aux4_HALF$type == "Feasible and Stable"] <- "Feasible and stable"
data_D_aux4_HALF$type[data_D_aux4_HALF$type == "Not feasible"] <- "Exclusion"

####################################################################
# MERGE ALL DATA

example_data <- dplyr::bind_rows(data_D_aux2,data_D_aux3,data_D_aux4,
                                 data_D_aux2_PERFECT,data_D_aux3_PERFECT,data_D_aux4_PERFECT,
                                 data_D_aux2_HALF,data_D_aux3_HALF,data_D_aux4_HALF)

####################################################################
# Plot data

# Modify the dataset to include LaTeX-compatible expressions
example_data$D_label <- (paste0("D[i]:", example_data$D))
example_data$alpha_label <- (paste0("-alpha[ij]:", example_data$alpha_ij))

ggplot(example_data, aes(x = r1A, y = r2A, fill = as.factor(type))) +
  geom_tile() +
  labs(
       x = TeX(paste0("$r_{1A}$")),
       y = TeX(paste0("$r_{2A}$"))) +
  scale_fill_manual(values = c("Feasible and stable" = "#3B9AB2",  
                               "Feasible and not stable" = "#E4D00A",
                               "Exclusion" = "#F21A00"),
                    name = "")+
  facet_grid(rows = vars(D_label), cols = vars(alpha_label), 
             labeller = label_parsed, switch = "both") +  # Moves labels outside
  coord_fixed() +
  theme_bw()+theme(legend.position="bottom")+
  theme(legend.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18),
        strip.background = element_rect(fill = "grey", color = "black"),
        panel.spacing = unit(1, "lines"))



png("Figures/Fig_Suppl_D1.png",
    width = 1.5*500*6.5, # The width of the plot in inches
    height = 1.5*520*4.4, res=300)
ggplot(example_data, aes(x = r1A, y = r2A, fill = as.factor(type))) +
  geom_tile() +
  labs(
    x = TeX(paste0("$r_{1A}$")),
    y = TeX(paste0("$r_{2A}$"))) +
  scale_fill_manual(values = c("Stable coexistence" = "#3B9AB2",  
                               "Unstable coexistence" = "#E4D00A",
                               "Exclusion" = "#F21A00"),
                    name = "")+
  facet_grid(rows = vars(D_label), cols = vars(alpha_label), 
             labeller = label_parsed, switch = "both") +  # Moves labels outside
  coord_fixed() +
  theme_bw()+theme(legend.position="bottom")+
  theme(legend.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18),
        strip.background = element_rect(fill = "grey", color = "black"),
        panel.spacing = unit(1, "lines"))

dev.off()

