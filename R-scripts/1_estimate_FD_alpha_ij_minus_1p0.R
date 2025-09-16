
library(tidyverse)
library(parallel)


source("R-scripts/aux_functions/eval_global_stability.R")
source("R-scripts/aux_functions/number_feasible_solutions_2sp_toy_DOM_ZERO_MASTER_V2.R")
source("R-scripts/aux_functions/number_feasible_solutions_2sp_toy_DOM_ZERO_4_values_V3.R")
source("R-scripts/aux_functions/number_feasible_solutions_2sp_toy_DOM_ZERO_2_values_V2.R")
####################################################################
# Definition of the diffusion/dispersion rates: D1 = D2 = D
D_vector <- c(0.001,0.01,0.1,1.0) # c(0.01,0.1)

####################################################################
# Create grid for r1 and r2, X and Y

A11 = -1
A12 = -1
A21 = -1
A22 = -1

####################################################################
# Growth rates to explore: r1, r2, 
r1 <- seq(-0.005, 0.005, by = 0.005) # seq(-0.1, 0.1, by = 0.005) 
r2 <- r1
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
                                       r2A = r2, 
                                       r2B = r2, 
                                       D = D_vector[D_i])
  
  # Agregar columnas de resultados
  data_growth_rates <- data_growth_rates %>%
    mutate(number_feasible_sol = NA,
           number_feasible_stable_sol = NA)
  
  # Estandarizar combinaciones para eliminar duplicados
  data_growth_rates <- data_growth_rates %>%
    mutate(
      r1A_ordered = pmin(r1A, r1B),  # Ordenar los pares (r1A, r1B)
      r1B_ordered = pmax(r1A, r1B),
      r2A_ordered = pmin(r2A, r2B),  # Ordenar los pares (r2A, r2B)
      r2B_ordered = pmax(r2A, r2B)
    ) %>%
    distinct(r1A_ordered, r1B_ordered, r2A_ordered, r2B_ordered, D, .keep_all = TRUE) %>%
    dplyr::select(-r1A_ordered, -r1B_ordered, -r2A_ordered, -r2B_ordered)  # Eliminar columnas auxiliares

  
  
  # Parallel calculations
  
  clusterExport(cl, list("number_feasible_solutions_2sp_toy_DOM_ZERO_4_values",
                         "number_feasible_solutions_2sp_toy_DOM_ZERO_2_values",
                         "eval_global_stability",
                         "number_feasible_and_stable_solutions_2sp_toy_DOM_ZERO_MASTER",
                         "data_growth_rates"))
  
  results <- parLapply(cl, 1:nrow(data_growth_rates), function(i) {
    number_feasible_and_stable_solutions_2sp_toy_DOM_ZERO_MASTER(data_growth_rates$D[i],
                                                                 data_growth_rates$r1A[i],
                                                                 data_growth_rates$r1B[i],
                                                                 data_growth_rates$r2A[i],
                                                                 data_growth_rates$r2B[i])
  })

  # Save number of feasible solutions
  
  results_matrix <- do.call(rbind, results)
  
  data_growth_rates[c("number_feasible_sol","number_feasible_stable_sol")] <- results_matrix
  
  
  
  # Commented for security reasons
  readr::write_csv(data_growth_rates,paste0("Results/FD_2sp_toy_DOM_ZERO_V5_",(1/D_vector[D_i]),"xD.csv"))
  
}

# Detiene el cluster
stopCluster(cl)


