
number_feasible_solutions_2sp_toy_DOM_HALF_all_values <-  function(r1A,r1B,r2A,r2B,D){

  
  N2B_possible <- possible_N2B_HALF(r1A,r1B,r2A,r2B,D)
  N2B = N2B_possible[round(Im(N2B_possible),16)==0] %>% Re()
  N2A = possible_N2A_HALF(N2B,r1A,r1B,r2A,r2B,D)
  N1A = 2*(r2A*N2A-N2A*N2A+D*(N2B-N2A))/N2A
  N1B = 2*(r2B*N2B-N2B*N2B-D*(N2B-N2A))/N2B
  
  solutions <- tibble(N1A=N1A,N1B=N1B,
                      N2A=N2A,N2B=N2B) %>%
    mutate(condition = N1A>0 & N1B>0 & N2A>0 & N2B>0)
  
  number_feasible_conditions <- sum(solutions$condition)
  
  #print(solutions)
  
  if(number_feasible_conditions == 0){
    return(c(0,0))
  }else{
    
    stable_eq_abundances_LV <- 0
    
    # Perfect diagonal dominance
    
    A11 = -1
    A12 = -0.5
    A21 = -0.5
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
