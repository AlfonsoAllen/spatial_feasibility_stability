
number_feasible_solutions_2sp_toy_DOM_PEREFCT_equal_rA <-  function(D,r1A,r1B,r2A,r2B){

  
  # Caso A N1A = N1B = r1
  
  if(r1A<=0){
    
    # z0_N1A <- 2*D^2 - D*r1A
    # z1_N1A <- (2*D - r1A)
    # pol_N1A <- polyroot(c(z0_N1A,z1_N1A,1)) %>% Re()
    # 
    # # N1A_possible_1 = N1B_possible_2 # Both roots must be positive to be a feasible solution
    # 
    # N1A_possible = -2*D - pol_N1A + r1A
    # N1B_possible  = pol_N1A
    
   
    return(c(0,0))
    
  }else{
    
    N1A = rep(r1A,3)
    N1B = rep(r1A,3)
    
    z0_N2B <- - r2A*D^2 - D^2*r2B + r2A*D*r2B
    z1_N2B <- (2*D^2 - D*r2A - 2*D*r2B + r2B^2)
    z2_N2B <- (2*D - 2*r2B)
    N2B_possible <- polyroot(c(z0_N2B,z1_N2B,z2_N2B,1)) %>% Re()
    N2A_possible <- N2B_possible*(D-r2B+N2B_possible)/D %>% Re()
   
    cond_1_feasible <- N2A_possible[1]>0 & N2B_possible[1]>0
    cond_2_feasible <- N2A_possible[2]>0 & N2B_possible[2]>0
    cond_3_feasible <- N2A_possible[3]>0 & N2B_possible[3]>0
    
    number_feasible_conditions <- cond_1_feasible + cond_2_feasible + cond_3_feasible
    
    if(number_feasible_conditions == 0){
      return(c(0,0))
    }else{
    
      stable_eq_abundances_LV <- 0
      
      # Perfect diagonal dominance
      
      A11 = -1
      A12 = 0
      A21 = 0
      A22 = -1
      
      if(cond_1_feasible){
        stable_eq_abundances_LV <- stable_eq_abundances_LV + 
          eval_global_stability(r1A,r1B,r2A,r2B,A11,A12,A21,A22,N1A[1],N1B[1],N2A[1],N2B[1],D)
      }
      
      if(cond_2_feasible){
        stable_eq_abundances_LV <- stable_eq_abundances_LV + 
          eval_global_stability(r1A,r1B,r2A,r2B,A11,A12,A21,A22,N1A[2],N1B[2],N2A[2],N2B[2],D)
      }
      
      if(cond_3_feasible){
        stable_eq_abundances_LV <- stable_eq_abundances_LV + 
          eval_global_stability(r1A,r1B,r2A,r2B,A11,A12,A21,A22,N1A[3],N1B[3],N2A[3],N2B[3],D)
      }
      
      return(c(number_feasible_conditions,stable_eq_abundances_LV))
        
    }
    
      
  }


}
