library(ggplot2)
library(dplyr)
library(readr)
library(latex2exp)
####################################################################
# Create grid for r1 and r2, X and Y

####################################################################
# Load data
data_HR <- readr::read_csv("Results/FD_2sp_toy_HIGH_RESOLUTION_r1B0p08_r2B0p055_1xD_ZOOM_SUBSET.csv")
data_HR_ZOOM <- readr::read_csv("Results/FD_2sp_toy_HIGH_RESOLUTION_r1B0p08_r2B0p055_1xD_NEW_ZOOM.csv") %>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0)


data_HR$type[data_HR$type == "Feasible"] <- "Unstable coexistence"
data_HR$type[data_HR$type == "Feasible and Stable"] <- "Stable coexistence"
data_HR$type[data_HR$type == "Not feasible"] <- "Exclusion"

data_HR_ZOOM$type <- "Not feasible"
data_HR_ZOOM$type[data_HR_ZOOM$Feasible == TRUE] <- "Feasible"
data_HR_ZOOM$type[data_HR_ZOOM$Feasible_stable == TRUE] <- "Feasible and Stable"

data_HR_ZOOM$type[data_HR_ZOOM$type == "Feasible"] <- "Unstable coexistence"
data_HR_ZOOM$type[data_HR_ZOOM$type == "Feasible and Stable"] <- "Stable coexistence"
data_HR_ZOOM$type[data_HR_ZOOM$type == "Not feasible"] <- "Exclusion"
####################################################################
# Plot data

plot_ZOOM_1 <- ggplot(data_HR %>% filter(r2A> -0.0225,r2A< -0.0125), aes(x = r1A, y = r2A, fill = as.factor(type))) +
  geom_tile() +
  labs(title = TeX(paste0("a) ","$D_{1}=D_{2}=", unique(data_HR$D),
                          "$")),
       x = TeX(paste0("$r_{1A}$")),
       y = TeX(paste0("$r_{2A}$")),
       fill= "") +
  scale_fill_manual(values = c("Stable coexistence" = "#3B9AB2",  
                               "Unstable coexistence" = "#E4D00A",
                               "Exclusion" = "#F21A00"),
                    name = "")+
  coord_fixed() +
  theme_bw()+theme(legend.position="bottom")+
  theme(legend.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))

plot_ZOOM_2 <- ggplot(data_HR_ZOOM, aes(x = r1A, y = r2A, fill = as.factor(type))) +
  geom_tile() +
  labs(title = TeX(paste0("b) ","$D_{1}=D_{2}=", unique(data_HR$D),
                          "$")),
       x = TeX(paste0("$r_{1A}$")),
       y = TeX(paste0("$r_{2A}$")),
       fill= "") +
  scale_fill_manual(values = c("Stable coexistence" = "#3B9AB2",  
                               "Unstable coexistence" = "#E4D00A",
                               "Exclusion" = "#F21A00"),
                    name = "")+
  coord_fixed() +
  theme_bw()+theme(legend.position="bottom")+
  theme(legend.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))


library(patchwork)

png("Figures/Fig_Suppl_D2.png",
    width = 500*11.5, # The width of the plot in inches
    height = 520*7.5, res=600)

((plot_ZOOM_1 + plot_ZOOM_2)+
    plot_layout(guides = "collect") & theme(legend.position = "bottom"))

dev.off()
