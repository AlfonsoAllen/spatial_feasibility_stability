
number_feasible_solutions_2sp_toy_DOM_PERFECT_all_values <-  function(D,r1A,r1B,r2A,r2B){

  z0_N2B <- (- r2A*D^2 - D^2*r2B + r2A*D*r2B)
  z1_N2B <- (2*D^2 - D*r2A - 2*D*r2B + r2B^2)
  z2_N2B <- (2*D - 2*r2B)
  
  N2B_possible <- polyroot(c(z0_N2B,z1_N2B,z2_N2B,1))
  N2B = N2B_possible[round(Im(N2B_possible),10)==0] %>% Re()
  
  z0_N1B <- (- r1A*D^2 - D^2*r1B + r1A*D*r1B)
  z1_N1B <- (2*D^2 - D*r1A - 2*D*r1B + r1B^2)
  z2_N1B <- (2*D - 2*r1B)
  
  N1B_possible <- polyroot(c(z0_N1B,z1_N1B,z2_N1B,1))
  N1B = N1B_possible[round(Im(N1B_possible),10)==0] %>% Re()
  
  solutions <- expand.grid(N1B,N2B) %>%
    rename(N1B=Var1,N2B=Var2) %>%
    mutate(N1A = N1B*(D - r1B + N1B)/D,
           N2A = N2B*(D - r2B + N2B)/D,
           condition = N1A>0 & N1B>0 & N2A>0 & N2B>0)
  
  
  
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
    
    for(i in 1:nrow(solutions)){
      N1A <- solutions$N1A[i]
      N1B <- solutions$N1B[i]
      N2A <- solutions$N2A[i]
      N2B <- solutions$N2B[i]
      
      stable_eq_abundances_LV <- stable_eq_abundances_LV + 
        eval_global_stability(r1A,r1B,r2A,r2B,A11,A12,A21,A22,N1A,N1B,N2A,N2B,D)
    }
    
    return(c(number_feasible_conditions,stable_eq_abundances_LV))
    
  }
  
}