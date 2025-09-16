
library(dplyr)
library(terra)
library(landscapemetrics)
####################################################################
# Create grid for r1 and r2, X and Y

A11 = -1
A12 = -1
A21 = -1
A22 = -1

# Load data

r1B <- seq(-0.25, 0.25, by = 0.005) 
D_aux1  <-  0.001
D_aux2  <-  0.01
D_aux3  <-  0.1
D_aux4  <-  1.0

data_D_aux1 <- readr::read_csv("Results/FD_2sp_toy_DOM_ZERO_V5_1000xD.csv") %>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0,
         r1B=round(r1B,3),
         r2B=round(r2B,3)) %>%
  filter(D == D_aux1)

data_D_aux1$type <- 1 # "Not feasible"
data_D_aux1$type[data_D_aux1$Feasible == TRUE] <- 2 # "Feasible"
data_D_aux1$type[data_D_aux1$Feasible_stable == TRUE] <- 3 # "Feasible and Stable"

data_D_aux2 <- readr::read_csv("Results/FD_2sp_toy_DOM_ZERO_V5_100xD.csv") %>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0,
         r1B=round(r1B,3),
         r2B=round(r2B,3)) %>%
  filter(D == D_aux2)

data_D_aux2$type <- 1 # "Not feasible"
data_D_aux2$type[data_D_aux2$Feasible == TRUE] <- 2 # "Feasible"
data_D_aux2$type[data_D_aux2$Feasible_stable == TRUE] <- 3 # "Feasible and Stable"


data_D_aux3 <- readr::read_csv("Results/FD_2sp_toy_DOM_ZERO_V5_10xD.csv") %>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0,
         r1B=round(r1B,3),
         r2B=round(r2B,3)) %>%
  filter(D == D_aux3)

data_D_aux3$type <- 1 # "Not feasible"
data_D_aux3$type[data_D_aux3$Feasible == TRUE] <- 2 # "Feasible"
data_D_aux3$type[data_D_aux3$Feasible_stable == TRUE] <- 3 # "Feasible and Stable"

data_D_aux4 <- readr::read_csv("Results/FD_2sp_toy_DOM_ZERO_V5_1xD.csv") %>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0,
         r1B=round(r1B,3),
         r2B=round(r2B,3)) %>%
  filter(D == D_aux4)

data_D_aux4$type <- 1 # "Not feasible"
data_D_aux4$type[data_D_aux4$Feasible == TRUE] <- 2 # "Feasible"
data_D_aux4$type[data_D_aux4$Feasible_stable == TRUE] <- 3 # "Feasible and Stable"


result_metrics_D_aux1 <- expand.grid(r1B = r1B, 
                              r2B = r1B, 
                              D = D_aux1)
result_metrics_D_aux1$shape_mn <- NA
result_metrics_D_aux1$relmutinf <- NA
result_metrics_D_aux1$marg_ent <- NA
result_metrics_D_aux1$perc_feasible <- NA
result_metrics_D_aux1$perc_feasible_stable <- NA

result_metrics_D_aux2 <- result_metrics_D_aux1
result_metrics_D_aux2$D <- D_aux2

result_metrics_D_aux3 <- result_metrics_D_aux1
result_metrics_D_aux3$D <- D_aux3

result_metrics_D_aux4 <- result_metrics_D_aux1
result_metrics_D_aux4$D <- D_aux4

for (i in 1:nrow(result_metrics_D_aux1)) {
  
  r1B_aux <- round(result_metrics_D_aux1$r1B[i],3)
  r2B_aux <- round(result_metrics_D_aux1$r2B[i],3)
  
  filtered_data_D_aux1 <- data_D_aux1[data_D_aux1$r1B == r1B_aux & data_D_aux1$r2B == r2B_aux, ]
  filtered_data_D_aux2 <- data_D_aux2[data_D_aux2$r1B == r1B_aux & data_D_aux2$r2B == r2B_aux, ]
  filtered_data_D_aux3 <- data_D_aux3[data_D_aux3$r1B == r1B_aux & data_D_aux3$r2B == r2B_aux, ]
  filtered_data_D_aux4 <- data_D_aux4[data_D_aux4$r1B == r1B_aux & data_D_aux4$r2B == r2B_aux, ]
  
  # Convertir el data.frame en un SpatRaster
  terra_data_D_aux1 <- terra::rast(filtered_data_D_aux1[, c("r1A", "r2A", "type")], type="xyz")
  terra_data_D_aux2 <- terra::rast(filtered_data_D_aux2[, c("r1A", "r2A", "type")], type="xyz")
  terra_data_D_aux3 <- terra::rast(filtered_data_D_aux3[, c("r1A", "r2A", "type")], type="xyz")
  terra_data_D_aux4 <- terra::rast(filtered_data_D_aux4[, c("r1A", "r2A", "type")], type="xyz")
  

  #####################################################
  # Metrics for D_aux1
  #####################################################
  
  shape_mn_D_aux1 <- landscapemetrics::lsm_l_shape_mn(terra_data_D_aux1)
  relmutinf_D_aux1 <- landscapemetrics::lsm_l_relmutinf(terra_data_D_aux1, neighbourhood = 4, 
                                    ordered = TRUE, base = "log2")
  marg_ent_D_aux1 <- landscapemetrics::lsm_l_ent(terra_data_D_aux1, 
                                              neighbourhood = 4, base = "log2")
  
  result_metrics_D_aux1$shape_mn[i] <- shape_mn_D_aux1$value
  result_metrics_D_aux1$relmutinf[i] <- relmutinf_D_aux1$value
  result_metrics_D_aux1$marg_ent[i] <- marg_ent_D_aux1$value
  result_metrics_D_aux1$perc_feasible[i] <- 100*sum(filtered_data_D_aux1$Feasible)/length(r1B)/length(r1B)
  result_metrics_D_aux1$perc_feasible_stable[i] <- 100*sum(filtered_data_D_aux1$Feasible_stable)/length(r1B)/length(r1B)
  
  
  #####################################################
  # Metrics for D_aux2
  #####################################################
  shape_mn_D_aux2 <- landscapemetrics::lsm_l_shape_mn(terra_data_D_aux2)
  relmutinf_D_aux2 <- landscapemetrics::lsm_l_relmutinf(terra_data_D_aux2, neighbourhood = 4, 
                                                        ordered = TRUE, base = "log2")
  marg_ent_D_aux2 <- landscapemetrics::lsm_l_ent(terra_data_D_aux2, 
                                                 neighbourhood = 4, base = "log2")
  
  result_metrics_D_aux2$shape_mn[i] <- shape_mn_D_aux2$value
  result_metrics_D_aux2$relmutinf[i] <- relmutinf_D_aux2$value
  result_metrics_D_aux2$marg_ent[i] <- marg_ent_D_aux2$value
  result_metrics_D_aux2$perc_feasible[i] <- 100*sum(filtered_data_D_aux2$Feasible)/length(r1B)/length(r1B)
  result_metrics_D_aux2$perc_feasible_stable[i] <- 100*sum(filtered_data_D_aux2$Feasible_stable)/length(r1B)/length(r1B)
  
  
  #####################################################
  # Metrics for D_aux3
  #####################################################
  
  shape_mn_D_aux3 <- landscapemetrics::lsm_l_shape_mn(terra_data_D_aux3)
  relmutinf_D_aux3 <- landscapemetrics::lsm_l_relmutinf(terra_data_D_aux3, neighbourhood = 4, 
                                                        ordered = TRUE, base = "log2")
  marg_ent_D_aux3 <- landscapemetrics::lsm_l_ent(terra_data_D_aux3, 
                                                 neighbourhood = 4, base = "log2")
  
  result_metrics_D_aux3$shape_mn[i] <- shape_mn_D_aux3$value
  result_metrics_D_aux3$relmutinf[i] <- relmutinf_D_aux3$value
  result_metrics_D_aux3$marg_ent[i] <- marg_ent_D_aux3$value
  result_metrics_D_aux3$perc_feasible[i] <- 100*sum(filtered_data_D_aux3$Feasible)/length(r1B)/length(r1B)
  result_metrics_D_aux3$perc_feasible_stable[i] <- 100*sum(filtered_data_D_aux3$Feasible_stable)/length(r1B)/length(r1B)
  
  
  #####################################################
  # Metrics for D_aux4
  #####################################################

  shape_mn_D_aux4 <- landscapemetrics::lsm_l_shape_mn(terra_data_D_aux4)
  relmutinf_D_aux4 <- landscapemetrics::lsm_l_relmutinf(terra_data_D_aux4, neighbourhood = 4, 
                                                        ordered = TRUE, base = "log2")
  marg_ent_D_aux4 <- landscapemetrics::lsm_l_ent(terra_data_D_aux4, 
                                                 neighbourhood = 4, base = "log2")
  
  result_metrics_D_aux4$shape_mn[i] <- shape_mn_D_aux4$value
  result_metrics_D_aux4$relmutinf[i] <- relmutinf_D_aux4$value
  result_metrics_D_aux4$marg_ent[i] <- marg_ent_D_aux4$value
  result_metrics_D_aux4$perc_feasible[i] <- 100*sum(filtered_data_D_aux4$Feasible)/length(r1B)/length(r1B)
  result_metrics_D_aux4$perc_feasible_stable[i] <- 100*sum(filtered_data_D_aux4$Feasible_stable)/length(r1B)/length(r1B)
  
}

# Commented for security reasons
# write.csv(result_metrics_D_aux1,"Results/result_metrics_D_aux1.csv")
# write.csv(result_metrics_D_aux2,"Results/result_metrics_D_aux2.csv")
# write.csv(result_metrics_D_aux3,"Results/result_metrics_D_aux3.csv")
# write.csv(result_metrics_D_aux4,"Results/result_metrics_D_aux4.csv")

result_metrics_D_aux1 <- readr::read_csv("Results/result_metrics_D_aux1.csv")
result_metrics_D_aux2 <- readr::read_csv("Results/result_metrics_D_aux2.csv")
result_metrics_D_aux3 <- readr::read_csv("Results/result_metrics_D_aux3.csv")
result_metrics_D_aux4 <- readr::read_csv("Results/result_metrics_D_aux4.csv")


library(ggplot2)
library(latex2exp)
library(wesanderson)


# min_value_fill_percen <- min(min(result_metrics_big$perc_feasible_stable),
#                              min(result_metrics$perc_feasible_stable))
# 
# max_value_fill_percen <- max(max(result_metrics_big$perc_feasible_stable),
#                              max(result_metrics$perc_feasible_stable))


plot_percen_D_aux1 <- ggplot(result_metrics_D_aux1, 
                             aes(x = r1B, y = r2B, fill = perc_feasible_stable)) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux1,
                          "$")),
       x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill= "Feasible+stable\npoints (%)") +
  scale_fill_gradientn(colours = wes_palette("Zissou1")) +
  coord_fixed() +
  theme_bw()


plot_percen_D_aux2 <- ggplot(result_metrics_D_aux2, 
                             aes(x = r1B, y = r2B, fill = perc_feasible_stable)) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux2,
                          "$")),
       x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill= "Feasible+stable\npoints (%)") +
  scale_fill_gradientn(colours = wes_palette("Zissou1")) +
  coord_fixed() +
  theme_bw()

plot_percen_D_aux3 <- ggplot(result_metrics_D_aux3, 
                             aes(x = r1B, y = r2B, fill = perc_feasible_stable)) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux3,
                          "$")),
       x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill= "Feasible+stable\npoints (%)") +
  scale_fill_gradientn(colours = wes_palette("Zissou1")) +
  coord_fixed() +
  theme_bw()

plot_percen_D_aux4 <- ggplot(result_metrics_D_aux4, 
                             aes(x = r1B, y = r2B, fill = perc_feasible_stable)) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux4,
                          "$")),
       x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill= "Feasible+stable\npoints (%)") +
  scale_fill_gradientn(colours = wes_palette("Zissou1")) +
  coord_fixed() +
  theme_bw()

###########################################

min_value_fill_relmutinf <- min(min(result_metrics_D_aux1$relmutinf),
                                min(result_metrics_D_aux2$relmutinf),
                                min(result_metrics_D_aux3$relmutinf),
                                min(result_metrics_D_aux4$relmutinf))

max_value_fill_relmutinf <- max(max(result_metrics_D_aux1$relmutinf),
                                max(result_metrics_D_aux2$relmutinf),
                                max(result_metrics_D_aux3$relmutinf),
                                max(result_metrics_D_aux4$relmutinf))


plot_relmutinf_D_aux1 <- ggplot(result_metrics_D_aux1, 
                                aes(x = r1B, y = r2B, fill = relmutinf)) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux1,"$")),
       x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill = "Rel. Mut. Inf.") +
  scale_fill_gradientn(colours = wes_palette("Zissou1"), 
                       limits = c(min_value_fill_relmutinf, max_value_fill_relmutinf)) +
  coord_fixed() +theme_bw()

plot_relmutinf_D_aux2 <- ggplot(result_metrics_D_aux2, 
                                aes(x = r1B, y = r2B, fill = relmutinf)) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux2,"$")),
       x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill = "Rel. Mut. Inf.") +
  scale_fill_gradientn(colours = wes_palette("Zissou1"), 
                       limits = c(min_value_fill_relmutinf, max_value_fill_relmutinf)) +
  coord_fixed() +theme_bw()

plot_relmutinf_D_aux3 <- ggplot(result_metrics_D_aux3, 
                                aes(x = r1B, y = r2B, fill = relmutinf)) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux3,"$")),
       x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill = "Rel. Mut. Inf.") +
  scale_fill_gradientn(colours = wes_palette("Zissou1"), 
                       limits = c(min_value_fill_relmutinf, max_value_fill_relmutinf)) +
  coord_fixed() +theme_bw()


plot_relmutinf_D_aux4 <- ggplot(result_metrics_D_aux4, 
                                aes(x = r1B, y = r2B, fill = relmutinf)) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux4,"$")),
       x = TeX(paste0("$r_{1B}$")),
       y = TeX(paste0("$r_{2B}$")),
       fill = "Rel. Mut. Inf.") +
  scale_fill_gradientn(colours = wes_palette("Zissou1"), 
                       limits = c(min_value_fill_relmutinf, max_value_fill_relmutinf)) +
  coord_fixed() +theme_bw()

library(cowplot)

plot_grid(plot_percen_D_aux1, 
          plot_percen_D_aux2,
          plot_percen_D_aux3,
          plot_percen_D_aux4, align = "v", axis = "tb")

plot_grid(plot_relmutinf_D_aux1,
          plot_relmutinf_D_aux2,
          plot_relmutinf_D_aux3,
          plot_relmutinf_D_aux4, align = "v", axis = "tb")

