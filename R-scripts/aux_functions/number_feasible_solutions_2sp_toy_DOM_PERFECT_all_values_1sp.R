
number_feasible_solutions_2sp_toy_DOM_PERFECT_all_values_1sp <-  function(D,r1A,r1B){
  
  z0_N1B <- (- r1A*D^2 - D^2*r1B + r1A*D*r1B)
  z1_N1B <- (2*D^2 - D*r1A - 2*D*r1B + r1B^2)
  z2_N1B <- (2*D - 2*r1B)
  
  N1B_possible <- polyroot(c(z0_N1B,z1_N1B,z2_N1B,1))
  N1B = N1B_possible[round(Im(N1B_possible),10)==0] %>% Re()
  
  solutions <- tibble(N1B=N1B) %>%
    mutate(N1A = N1B*(D - r1B + N1B)/D,
           condition = N1A>0 & N1B>0)
  
  number_feasible_conditions <- sum(solutions$condition)
  
  if(number_feasible_conditions == 0){
    return(c(0,0))
  }else{
    
    stable_eq_abundances_LV <- 0
    
    # Perfect diagonal dominance
    
    A11 = -1
    A12 = 0
    A21 = 0
    A22 = -1
    
    solutions_filtered <- solutions %>% filter(condition==TRUE)
    
    for(i in 1:nrow(solutions_filtered)){
      N1A <- solutions_filtered$N1A[i]
      N1B <- solutions_filtered$N1B[i]
      N2A <- solutions_filtered$N2A[i]
      N2B <- solutions_filtered$N2B[i]
      
      stable_eq_abundances_LV <- stable_eq_abundances_LV + 
        eval_global_stability(r1A,r1B,r2A,r2B,
                              A11,A12,A21,A22,
                              N1A,N1B,N2A,N2B,D)
    }
    
    return(c(number_feasible_conditions,stable_eq_abundances_LV))
    
  }
  
}