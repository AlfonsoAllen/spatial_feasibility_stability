
library(tidyverse)
library(parallel)


source("R-scripts/aux_functions/eval_global_stability_PERFECT_1sp.R")
source("R-scripts/aux_functions/number_feasible_and_stable_solutions_2sp_toy_DOM_PERFECT_MASTER_1sp.R")
source("R-scripts/aux_functions/number_feasible_solutions_2sp_toy_DOM_PERFECT_all_values_1sp.R")
####################################################################
# Definition of the diffusion/dispersion rates: D1 = D2 = D
D_vector <- c(0.001,0.01,0.1,1.0) # c(0.01,0.1)

####################################################################
# Create grid for r1 and r2, X and Y

A11 = -1
A12 = 0
A21 = 0
A22 = -1

####################################################################
# Growth rates to explore: r1, r2, 
r1 <- seq(-0.1, 0.1, by = 0.0005) # seq(-0.005, 0.005, by = 0.005) # 
####################################################################
# Register cores for parallelization
n_cores <- 9 #12

cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(tidyverse))


####################################################################


for(D_i in 1:length(D_vector)){
  
  # Create grid for r1A, r1B, r2A, and r2B

  data_growth_rates <- expand.grid(r1A = r1, 
                                       r1B = r1,
                                       D = D_vector[D_i])
  
  # Agregar columnas de resultados
  data_growth_rates <- data_growth_rates %>%
    mutate(number_feasible_sol = NA,
           number_feasible_stable_sol = NA)
  
  # Estandarizar combinaciones para eliminar duplicados
  # data_growth_rates <- data_growth_rates %>%
  #   mutate(
  #     r1A_ordered = pmin(r1A, r1B),  # Ordenar los pares (r1A, r1B)
  #     r1B_ordered = pmax(r1A, r1B),
  #     r2A_ordered = pmin(r2A, r2B),  # Ordenar los pares (r2A, r2B)
  #     r2B_ordered = pmax(r2A, r2B)
  #   ) %>%
  #   distinct(r1A_ordered, r1B_ordered, r2A_ordered, r2B_ordered, D, .keep_all = TRUE) %>%
  #   dplyr::select(-r1A_ordered, -r1B_ordered, -r2A_ordered, -r2B_ordered)  # Eliminar columnas auxiliares

  
  
  # Parallel calculations
  
  clusterExport(cl, list("number_feasible_solutions_2sp_toy_DOM_PERFECT_all_values_1sp",
                         "eval_global_stability_PERFECT_1sp",
                         "number_feasible_and_stable_solutions_2sp_toy_DOM_PERFECT_MASTER_1sp",
                         "data_growth_rates"))
  
  results <- parLapply(cl, 1:nrow(data_growth_rates), function(i) {
    number_feasible_and_stable_solutions_2sp_toy_DOM_PERFECT_MASTER_1sp(data_growth_rates$D[i],
                                                                 data_growth_rates$r1A[i],
                                                                 data_growth_rates$r1B[i])
  })

  # Save number of feasible solutions
  
  results_matrix <- do.call(rbind, results)
  
  data_growth_rates[c("number_feasible_sol","number_feasible_stable_sol")] <- results_matrix
  
  
  
  # Commented for security reasons
  readr::write_csv(data_growth_rates,paste0("Results/FD_2sp_toy_DOM_PERFECT_V1_",(1/D_vector[D_i]),"xD_1sp.csv"))
  
}

# Detiene el cluster
stopCluster(cl)

D_aux1 = 0.001
data_D_aux1 <- readr::read_csv(paste0("Results/FD_2sp_toy_DOM_PERFECT_V1_",(1/D_aux1),"xD_1sp.csv"))%>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0)
data_D_aux1$type <- "Not feasible"
data_D_aux1$type[data_D_aux1$Feasible == TRUE] <- "Feasible"
data_D_aux1$type[data_D_aux1$Feasible_stable == TRUE] <- "Feasible and Stable"

plot_D_aux1 <- ggplot(data_D_aux1, aes(x = r1A, y = r1B, fill = as.factor(type))) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux1,
                          ",\\alpha_{11}=", 
                          "\\alpha_{22}=", A22,
                          ",\\alpha_{12}=", 
                          "\\alpha_{21}=", A21,
                          "$")),
       x = TeX(paste0("$r_{1A}$")),
       y = TeX(paste0("$r_{1B}$"))) +
  scale_fill_manual(values = c("Feasible and Stable" = "#3B9AB2", 
                               "Feasible" = "#E4D00A",            
                               "Not feasible" = "#F21A00"),
                    name = "") +
  coord_fixed() +  theme_bw()

D_aux2 = 0.01
data_D_aux2 <- readr::read_csv(paste0("Results/FD_2sp_toy_DOM_PERFECT_V1_",(1/D_aux2),"xD_1sp.csv"))%>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0)
data_D_aux2$type <- "Not feasible"
data_D_aux2$type[data_D_aux2$Feasible == TRUE] <- "Feasible"
data_D_aux2$type[data_D_aux2$Feasible_stable == TRUE] <- "Feasible and Stable"

plot_D_aux2 <- ggplot(data_D_aux2, aes(x = r1A, y = r1B, fill = as.factor(type))) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux2,
                          ",\\alpha_{11}=", 
                          "\\alpha_{22}=", A22,
                          ",\\alpha_{12}=", 
                          "\\alpha_{21}=", A21,
                          "$")),
       x = TeX(paste0("$r_{1A}$")),
       y = TeX(paste0("$r_{1B}$"))) +
  scale_fill_manual(values = c("Feasible and Stable" = "#3B9AB2", 
                               "Feasible" = "#E4D00A",            
                               "Not feasible" = "#F21A00"),
                    name = "") +
  coord_fixed() +  theme_bw()

D_aux3 = 0.1
data_D_aux3 <- readr::read_csv(paste0("Results/FD_2sp_toy_DOM_PERFECT_V1_",(1/D_aux3),"xD_1sp.csv"))%>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0)
data_D_aux3$type <- "Not feasible"
data_D_aux3$type[data_D_aux3$Feasible == TRUE] <- "Feasible"
data_D_aux3$type[data_D_aux3$Feasible_stable == TRUE] <- "Feasible and Stable"

plot_D_aux3 <- ggplot(data_D_aux3, aes(x = r1A, y = r1B, fill = as.factor(type))) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux3,
                          ",\\alpha_{11}=", 
                          "\\alpha_{22}=", A22,
                          ",\\alpha_{12}=", 
                          "\\alpha_{21}=", A21,
                          "$")),
       x = TeX(paste0("$r_{1A}$")),
       y = TeX(paste0("$r_{1B}$"))) +
  scale_fill_manual(values = c("Feasible and Stable" = "#3B9AB2", 
                               "Feasible" = "#E4D00A",            
                               "Not feasible" = "#F21A00"),
                    name = "") +
  coord_fixed() +  theme_bw()




D_aux4 = 1
data_D_aux4 <- readr::read_csv(paste0("Results/FD_2sp_toy_DOM_PERFECT_V1_",(1/D_aux4),"xD_1sp.csv"))%>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0)
data_D_aux4$type <- "Not feasible"
data_D_aux4$type[data_D_aux4$Feasible == TRUE] <- "Feasible"
data_D_aux4$type[data_D_aux4$Feasible_stable == TRUE] <- "Feasible and Stable"

plot_D_aux4 <- ggplot(data_D_aux4, aes(x = r1A, y = r1B, fill = as.factor(type))) +
  geom_tile() +
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux4,
                          ",\\alpha_{11}=",
                          "\\alpha_{22}=", A22,
                          ",\\alpha_{12}=",
                          "\\alpha_{21}=", A21,
                          "$")),
       x = TeX(paste0("$r_{1A}$")),
       y = TeX(paste0("$r_{1B}$"))) +
  scale_fill_manual(values = c("Feasible and Stable" = "#3B9AB2", 
                               "Feasible" = "#E4D00A",            
                               "Not feasible" = "#F21A00"),
                    name = "") +
  coord_fixed() +  theme_bw()


data_D_aux4 <- readr::read_csv(paste0("Results/FD_2sp_toy_DOM_PERFECT_V1_",(1/D_aux4),"xD_1sp.csv"))%>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0)
data_D_aux4$type <- "Not feasible"
data_D_aux4$type[data_D_aux4$Feasible == TRUE] <- "Feasible"
data_D_aux4$type[data_D_aux4$Feasible_stable == TRUE] <- "Feasible and Stable"

ggplot(data_D_aux4, aes(x = r1A, y = r1B, fill = as.factor(type))) +
  geom_tile() +
  geom_hline(yintercept = 0.05,linewidth=2)+
  geom_vline(xintercept = -0.0525,linewidth=2,linetype="dashed")+
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux4,
                          ",\\alpha_{11}=",
                          "\\alpha_{22}=", A22,
                          ",\\alpha_{12}=",
                          "\\alpha_{21}=", A21,
                          "$")),
       x = TeX(paste0("$r_{2A}$")),
       y = TeX(paste0("$r_{2B}$"))) +
  scale_fill_manual(values = c("Feasible and Stable" = "#3B9AB2", 
                               "Feasible" = "#E4D00A",            
                               "Not feasible" = "#F21A00"),
                    name = "") +
  coord_fixed() +  theme_bw()

ggplot(data_D_aux4, aes(x = r1A, y = r1B, fill = as.factor(type))) +
  geom_tile() +
  geom_hline(yintercept = 0.025,linewidth=2)+
  geom_vline(xintercept = -0.025,linewidth=2,linetype="dashed")+
  labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux4,
                          ",\\alpha_{11}=",
                          "\\alpha_{22}=", A22,
                          ",\\alpha_{12}=",
                          "\\alpha_{21}=", A21,
                          "$")),
       x = TeX(paste0("$r_{1A}$")),
       y = TeX(paste0("$r_{1B}$"))) +
  scale_fill_manual(values = c("Feasible and Stable" = "#3B9AB2", 
                               "Feasible" = "#E4D00A",            
                               "Not feasible" = "#F21A00"),
                    name = "") +
  coord_fixed() +  theme_bw()


library(patchwork)

((plot_D_aux1 + plot_D_aux2+plot_D_aux3 + plot_D_aux4)+
    plot_layout(guides = "collect") & theme(legend.position = "bottom"))
