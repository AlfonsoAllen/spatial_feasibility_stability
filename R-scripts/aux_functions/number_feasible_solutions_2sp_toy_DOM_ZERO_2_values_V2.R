
number_feasible_solutions_2sp_toy_DOM_ZERO_2_values <-  function(D,r1A,r1B,r2A,r2B){
  
  A = 2*D + r1A-r2A
  
  if(A*A-4*A*D>=0){
    
    pol_root <- polyroot(c(A*D,-A,1)) %>% Re()
    pol_root
    # Abundances at equilibrium: N2B
    
    N2B_possible <- pol_root * r2A / (r1A-r2A)
    N1B_possible <- -1*(pol_root-r1A+r2A) * r1A/ (r1A-r2A)
    
    N2A_possible <- (A - pol_root) * r2A / (r1A-r2A)
    N1A_possible <- -1*(2*D - pol_root) * r1A/ (r1A-r2A)
    
    cond_1 <- N2A_possible[1]>0 & N1A_possible[1]>0 & N2B_possible[1]>0 & N1B_possible[1]>0
    cond_2 <- N2A_possible[2]>0 & N1A_possible[2]>0 & N2B_possible[2]>0 & N1B_possible[2]>0
    
    if(cond_1>0){
      
      A11 = -1
      A12 = -1
      A21 = -1
      A22 = -1
      
      cond_3 <- eval_global_stability(r1A,r1B,r2A,r2B,A11,A12,A21,A22,
                                      N1A_possible[1],N1B_possible[1],
                                      N2A_possible[1],N2B_possible[1],D)
    }else{
      cond_3 <- 0
    }
    
    if(cond_2>0){
      
      A11 = -1
      A12 = -1
      A21 = -1
      A22 = -1
      
      cond_4 <- eval_global_stability(r1A,r1B,r2A,r2B,A11,A12,A21,A22,
                                      N1A_possible[2],N1B_possible[2],
                                      N2A_possible[2],N2B_possible[2],D)
    }else{
      cond_4 <- 0
    }
    
    return(c((cond_1 + cond_2), (cond_3 + cond_4)))
    
  }else{
    return(c(0,0))
  }
  
}
