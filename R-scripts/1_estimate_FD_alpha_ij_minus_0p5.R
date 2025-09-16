
library(tidyverse)
library(parallel)


source("R-scripts/aux_functions/eval_global_stability.R")
source("R-scripts/aux_functions/number_feasible_solutions_2sp_toy_DOM_HALF_MASTER_V1.R")
source("R-scripts/aux_functions/possible_N2A_HALF.R")
source("R-scripts/aux_functions/possible_N2B_HALF.R")
source("R-scripts/aux_functions/number_feasible_solutions_2sp_toy_DOM_HALF_all_values.R")
####################################################################
# Definition of the diffusion/dispersion rates: D1 = D2 = D
D_vector <- c(0.001,0.01,0.1,1.0) # c(0.01,0.1)

####################################################################
# Create grid for r1 and r2, X and Y

A11 = -1
A12 = -0.5
A21 = -0.5
A22 = -1

####################################################################
# Growth rates to explore: r1, r2, 
r1 <- seq(-0.1, 0.1, by = 0.005) # seq(-0.005, 0.005, by = 0.005) # 
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
  
  # 
  # for(i in 1:nrow(data_growth_rates)){
  #   print(i)
  #   number_feasible_and_stable_solutions_2sp_toy_DOM_HALF_MASTER(data_growth_rates$D[i],
  #                                                         data_growth_rates$r1A[i],
  #                                                         data_growth_rates$r1B[i],
  #                                                         data_growth_rates$r2A[i],
  #                                                         data_growth_rates$r2B[i])
  # }
  # 
  
  # Parallel calculations
  
  clusterExport(cl, list("number_feasible_solutions_2sp_toy_DOM_HALF_all_values",
                         "eval_global_stability", "possible_N2A_HALF", "possible_N2B_HALF",
                         "number_feasible_and_stable_solutions_2sp_toy_DOM_HALF_MASTER",
                         "data_growth_rates"))
  
  results <- parLapply(cl, 1:nrow(data_growth_rates), function(i) {
    number_feasible_and_stable_solutions_2sp_toy_DOM_HALF_MASTER(data_growth_rates$D[i],
                                                                 data_growth_rates$r1A[i],
                                                                 data_growth_rates$r1B[i],
                                                                 data_growth_rates$r2A[i],
                                                                 data_growth_rates$r2B[i])
  })

  # Save number of feasible solutions
  
  results_matrix <- do.call(rbind, results)
  
  data_growth_rates[c("number_feasible_sol","number_feasible_stable_sol")] <- results_matrix
  
  
  
  # Commented for security reasons
  readr::write_csv(data_growth_rates,paste0("Results/FD_2sp_toy_DOM_HALF_",(1/D_vector[D_i]),"xD.csv"))
  
}

# Detiene el cluster
stopCluster(cl)


