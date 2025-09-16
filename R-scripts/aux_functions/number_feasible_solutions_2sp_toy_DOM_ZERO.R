
number_feasible_solutions_2sp_toy_DOM_ZERO <-  function(D,r1A,r1B,r2A,r2B){
  
  ####################################################################
  # Feasible abundances at equilibrium (Maple): N2B
  N2B_z1_1 <- D*r1A^2*r1B^2*r2A*r2B - 2*D*r1A^2*r1B*r2A*r2B^2 - 
    2*D*r1A*r1B^2*r2A^2*r2B + 4*D*r1A*r1B*r2A^2*r2B^2 + 2*D^2*r1A^2*r1B*r2A*r2B + 
    2*D^2*r1A*r1B^2*r2A*r2B - 4*D^2*r1A*r1B*r2A^2*r2B - 4*D^2*r1A*r1B*r2A*r2B^2 + 
    D*r1A^2*r2A*r2B^3 - 2*D*r1A*r2A^2*r2B^3 + D*r1B^2*r2A^3*r2B - 
    2*D*r1B*r2A^3*r2B^2 + 2*D^4*r1A*r1B*r2A + 2*D^4*r1A*r1B*r2B - 
    2*D^4*r1A*r2A*r2B - 2*D^4*r1B*r2A*r2B + D^3*r1A^2*r1B*r2A - 
    2*D^3*r1A^2*r1B*r2B - D^3*r1A^2*r2A*r2B - 2*D^3*r1A*r1B^2*r2A + 
    D^3*r1A*r1B^2*r2B + D^3*r1A*r1B*r2A^2 + D^3*r1A*r1B*r2B^2 - 
    D^3*r1A*r2A^2*r2B + 2*D^3*r1A*r2A*r2B^2 - D^3*r1B^2*r2A*r2B + 
    2*D^3*r1B*r2A^2*r2B - D^3*r1B*r2A*r2B^2 + D^2*r1A^3*r1B*r2A - 
    D^2*r1A^3*r2A*r2B - D^2*r1A^2*r1B^2*r2A - D^2*r1A^2*r1B^2*r2B - 
    2*D^2*r1A^2*r1B*r2A^2 + 2*D^2*r1A^2*r1B*r2B^2 + 2*D^2*r1A^2*r2A^2*r2B - 
    D^2*r1A^2*r2A*r2B^2 + D^2*r1A*r1B^3*r2B + 2*D^2*r1A*r1B^2*r2A^2 - 
    2*D^2*r1A*r1B^2*r2B^2 + D^2*r1A*r1B*r2A^3 + D^2*r1A*r1B*r2B^3 - 
    D^2*r1A*r2A^3*r2B + 2*D^2*r1A*r2A^2*r2B^2 + 2*D^2*r1A*r2A*r2B^3 - 
    D^2*r1B^3*r2A*r2B - D^2*r1B^2*r2A^2*r2B + 2*D^2*r1B^2*r2A*r2B^2 + 
    2*D^2*r1B*r2A^3*r2B + 2*D^2*r1B*r2A^2*r2B^2 - D^2*r1B*r2A*r2B^3 - 
    D^4*r1A^2*r1B - 3*D^4*r1A^2*r2A + D^4*r1A^2*r2B - D^4*r1A*r1B^2 + 
    3*D^4*r1A*r2A^2 - D^4*r1A*r2B^2 + D^4*r1B^2*r2A - 3*D^4*r1B^2*r2B - 
    D^4*r1B*r2A^2 + 3*D^4*r1B*r2B^2 + D^4*r2A^2*r2B + D^4*r2A*r2B^2 -
    D^3*r1A^3*r1B + D^3*r1A^3*r2B + 2*D^3*r1A^2*r1B^2 - D^3*r1A*r1B^3 - 
    D^3*r1A*r2B^3 + D^3*r1B^3*r2A - D^3*r1B*r2A^3 + D^3*r2A^3*r2B - 
    2*D^3*r2A^2*r2B^2 + D^3*r2A*r2B^3 - D^2*r1A^2*r2B^3 - D^2*r1B^2*r2A^3 -
    D^2*r2A^3*r2B^2 - D^2*r2A^2*r2B^3 + D*r2A^3*r2B^3 + D^4*r1A^3 + 
    D^4*r1B^3 - D^4*r2A^3 - D^4*r2B^3
  
  N2B_z2_1 <- 0
  N2B_z3_1 <- 2*D^2*r1A^2*r1B^2 - 4*D^2*r1A^2*r1B*r2B + 2*D^2*r1A^2*r2B^2 - 
    2*D^2*r1A*r1B^3 - 4*D^2*r1A*r1B^2*r2A + 6*D^2*r1A*r1B^2*r2B + 
    8*D^2*r1A*r1B*r2A*r2B - 6*D^2*r1A*r1B*r2B^2 - 4*D^2*r1A*r2A*r2B^2 + 
    2*D^2*r1A*r2B^3 + 2*D^2*r1B^3*r2A + 2*D^2*r1B^2*r2A^2 - 6*D^2*r1B^2*r2A*r2B - 
    4*D^2*r1B*r2A^2*r2B + 6*D^2*r1B*r2A*r2B^2 + 2*D^2*r2A^2*r2B^2 - 
    2*D^2*r2A*r2B^3 - D*r1A^3*r1B^2 + 2*D*r1A^3*r1B*r2B - D*r1A^3*r2B^2 + 
    D*r1A^2*r1B^3 + D*r1A^2*r1B^2*r2A - 3*D*r1A^2*r1B^2*r2B - 
    2*D*r1A^2*r1B*r2A*r2B + 3*D*r1A^2*r1B*r2B^2 + D*r1A^2*r2A*r2B^2 - 
    D*r1A^2*r2B^3 - 2*D*r1A*r1B^3*r2A + D*r1A*r1B^2*r2A^2 + 
    6*D*r1A*r1B^2*r2A*r2B - 2*D*r1A*r1B*r2A^2*r2B - 6*D*r1A*r1B*r2A*r2B^2 +
    D*r1A*r2A^2*r2B^2 + 2*D*r1A*r2A*r2B^3 + D*r1B^3*r2A^2 - D*r1B^2*r2A^3 - 
    3*D*r1B^2*r2A^2*r2B + 2*D*r1B*r2A^3*r2B + 3*D*r1B*r2A^2*r2B^2 - D*r2A^3*r2B^2 - 
    D*r2A^2*r2B^3 - r1A^2*r1B^3*r2B + 3*r1A^2*r1B^2*r2B^2 - 3*r1A^2*r1B*r2B^3 + 
    r1A^2*r2B^4 + 2*r1A*r1B^3*r2A*r2B - 6*r1A*r1B^2*r2A*r2B^2 + 
    6*r1A*r1B*r2A*r2B^3 - 2*r1A*r2A*r2B^4 - r1B^3*r2A^2*r2B + 
    3*r1B^2*r2A^2*r2B^2 - 3*r1B*r2A^2*r2B^3 + r2A^2*r2B^4
  N2B_z4_1 <- 0
  N2B_z5_1 <- r1A^2*r1B^3 - 3*r1A^2*r1B^2*r2B + 3*r1A^2*r1B*r2B^2 - r1A^2*r2B^3 - 
    2*r1A*r1B^3*r2A + 6*r1A*r1B^2*r2A*r2B - 6*r1A*r1B*r2A*r2B^2 + 
    2*r1A*r2A*r2B^3 + r1B^3*r2A^2 - 3*r1B^2*r2A^2*r2B + 3*r1B*r2A^2*r2B^2 - 
    r2A^2*r2B^3
  
  
  
  
  ###################################################################
  # Feasible abundances at equilibrium (Maple): N1B
  N1B_z1_1 <- D*r1A^2*r1B^2*r2A*r2B - 2*D*r1A^2*r1B*r2A*r2B^2 - 2*D*r1A*r1B^2*r2A^2*r2B + 
    4*D*r1A*r1B*r2A^2*r2B^2 + 2*D^2*r1A^2*r1B*r2A*r2B + 
    2*D^2*r1A*r1B^2*r2A*r2B - 4*D^2*r1A*r1B*r2A^2*r2B - 
    4*D^2*r1A*r1B*r2A*r2B^2 + D*r1A^2*r2A*r2B^3 - 2*D*r1A*r2A^2*r2B^3 + 
    D*r1B^2*r2A^3*r2B - 2*D*r1B*r2A^3*r2B^2 + 2*D^4*r1A*r1B*r2A + 
    2*D^4*r1A*r1B*r2B - 2*D^4*r1A*r2A*r2B - 2*D^4*r1B*r2A*r2B + 
    D^3*r1A^2*r1B*r2A - 2*D^3*r1A^2*r1B*r2B - D^3*r1A^2*r2A*r2B - 
    2*D^3*r1A*r1B^2*r2A + D^3*r1A*r1B^2*r2B + D^3*r1A*r1B*r2A^2 + 
    D^3*r1A*r1B*r2B^2 - D^3*r1A*r2A^2*r2B + 2*D^3*r1A*r2A*r2B^2 - 
    D^3*r1B^2*r2A*r2B + 2*D^3*r1B*r2A^2*r2B - D^3*r1B*r2A*r2B^2 + 
    D^2*r1A^3*r1B*r2A - D^2*r1A^3*r2A*r2B - D^2*r1A^2*r1B^2*r2A - 
    D^2*r1A^2*r1B^2*r2B - 2*D^2*r1A^2*r1B*r2A^2 + 
    2*D^2*r1A^2*r1B*r2B^2 + 2*D^2*r1A^2*r2A^2*r2B - 
    D^2*r1A^2*r2A*r2B^2 + D^2*r1A*r1B^3*r2B + 2*D^2*r1A*r1B^2*r2A^2 - 
    2*D^2*r1A*r1B^2*r2B^2 + D^2*r1A*r1B*r2A^3 + D^2*r1A*r1B*r2B^3 - 
    D^2*r1A*r2A^3*r2B + 2*D^2*r1A*r2A^2*r2B^2 + 2*D^2*r1A*r2A*r2B^3 - 
    D^2*r1B^3*r2A*r2B - D^2*r1B^2*r2A^2*r2B + 2*D^2*r1B^2*r2A*r2B^2 + 
    2*D^2*r1B*r2A^3*r2B + 2*D^2*r1B*r2A^2*r2B^2 - D^2*r1B*r2A*r2B^3 - 
    D^4*r1A^2*r1B - 3*D^4*r1A^2*r2A + D^4*r1A^2*r2B - D^4*r1A*r1B^2 + 
    3*D^4*r1A*r2A^2 - D^4*r1A*r2B^2 + D^4*r1B^2*r2A - 3*D^4*r1B^2*r2B - 
    D^4*r1B*r2A^2 + 3*D^4*r1B*r2B^2 + D^4*r2A^2*r2B + D^4*r2A*r2B^2 - 
    D^3*r1A^3*r1B + D^3*r1A^3*r2B + 2*D^3*r1A^2*r1B^2 - D^3*r1A*r1B^3 - 
    D^3*r1A*r2B^3 + D^3*r1B^3*r2A - D^3*r1B*r2A^3 + D^3*r2A^3*r2B - 
    2*D^3*r2A^2*r2B^2 + D^3*r2A*r2B^3 - D^2*r1A^2*r2B^3 - D^2*r1B^2*r2A^3 - 
    D^2*r2A^3*r2B^2 - D^2*r2A^2*r2B^3 + D*r2A^3*r2B^3 + D^4*r1A^3 + 
    D^4*r1B^3 - D^4*r2A^3 - D^4*r2B^3
  N1B_z2_1 <- 0
  N1B_z3_1 <-(2*D^2*r1A^2*r1B^2 - 4*D^2*r1A^2*r1B*r2B + 2*D^2*r1A^2*r2B^2 - 
                2*D^2*r1A*r1B^3 - 4*D^2*r1A*r1B^2*r2A + 6*D^2*r1A*r1B^2*r2B + 
                8*D^2*r1A*r1B*r2A*r2B - 6*D^2*r1A*r1B*r2B^2 - 4*D^2*r1A*r2A*r2B^2 + 
                2*D^2*r1A*r2B^3 + 2*D^2*r1B^3*r2A + 2*D^2*r1B^2*r2A^2 - 
                6*D^2*r1B^2*r2A*r2B - 4*D^2*r1B*r2A^2*r2B + 6*D^2*r1B*r2A*r2B^2 + 
                2*D^2*r2A^2*r2B^2 - 2*D^2*r2A*r2B^3 - D*r1A^3*r1B^2 + 
                2*D*r1A^3*r1B*r2B - D*r1A^3*r2B^2 + D*r1A^2*r1B^3 + 
                D*r1A^2*r1B^2*r2A - 3*D*r1A^2*r1B^2*r2B - 2*D*r1A^2*r1B*r2A*r2B + 
                3*D*r1A^2*r1B*r2B^2 + D*r1A^2*r2A*r2B^2 - D*r1A^2*r2B^3 - 
                2*D*r1A*r1B^3*r2A + D*r1A*r1B^2*r2A^2 + 6*D*r1A*r1B^2*r2A*r2B - 
                2*D*r1A*r1B*r2A^2*r2B - 6*D*r1A*r1B*r2A*r2B^2 + D*r1A*r2A^2*r2B^2 + 
                2*D*r1A*r2A*r2B^3 + D*r1B^3*r2A^2 - D*r1B^2*r2A^3 - 
                3*D*r1B^2*r2A^2*r2B + 2*D*r1B*r2A^3*r2B + 3*D*r1B*r2A^2*r2B^2 - 
                D*r2A^3*r2B^2 - D*r2A^2*r2B^3 - r1A^2*r1B^3*r2B + 
                3*r1A^2*r1B^2*r2B^2 - 3*r1A^2*r1B*r2B^3 + r1A^2*r2B^4 + 
                2*r1A*r1B^3*r2A*r2B - 6*r1A*r1B^2*r2A*r2B^2 + 6*r1A*r1B*r2A*r2B^3 - 
                2*r1A*r2A*r2B^4 - r1B^3*r2A^2*r2B + 3*r1B^2*r2A^2*r2B^2 - 
                3*r1B*r2A^2*r2B^3 + r2A^2*r2B^4)
  N1B_z4_1 <- 0
  N1B_z5_1 <-(r1A^2*r1B^3 - 3*r1A^2*r1B^2*r2B + 3*r1A^2*r1B*r2B^2 - r1A^2*r2B^3 - 
                2*r1A*r1B^3*r2A + 6*r1A*r1B^2*r2A*r2B - 6*r1A*r1B*r2A*r2B^2 + 
                2*r1A*r2A*r2B^3 + r1B^3*r2A^2 - 3*r1B^2*r2A^2*r2B + 3*r1B*r2A^2*r2B^2 - 
                r2A^2*r2B^3)
  
  if(!all(round(c(N2B_z2_1,N2B_z3_1,N2B_z4_1,N2B_z5_1),10)==rep(0,4))){
    
    pol_N2B_roots_1 <- polyroot(c(N2B_z1_1,N2B_z2_1,N2B_z3_1,N2B_z4_1,N2B_z5_1))
    pol_N2B_roots_1 
    # Abundances at equilibrium: N2B
    N2B_possible <- pol_N2B_roots_1^2
    feasible_solutions_N2B <- which(round(Im(N2B_possible),10) == 0 & Re(N2B_possible) > 0)
    N2B_feasible <- Re(N2B_possible[feasible_solutions_N2B]) %>% unique()
    N2B_feasible
    
    
    pol_N1B_roots_1 <- polyroot(c(N1B_z1_1,N1B_z2_1,N1B_z3_1,N1B_z4_1,N1B_z5_1))
    pol_N1B_roots_1
    
    N1B_z1_12 <- -D*r1A^2*r1B + 
      D*pol_N1B_roots_1^2*r1A*r2B + 
      D*pol_N1B_roots_1^2*r1B*r2A - 
      D*pol_N1B_roots_1^2*r2A*r2B - 
      pol_N1B_roots_1^2*r1A*r1B*r2B + 
      pol_N1B_roots_1^2*r1B*r2A*r2B - 
      2*D*pol_N1B_roots_1^2*r1A*r2A - 
      D*pol_N1B_roots_1^2*r1A*r1B + 
      D*pol_N1B_roots_1^2*r1A^2 + 
      D*pol_N1B_roots_1^2*r2A^2 + 
      pol_N1B_roots_1^2*r1A*r1B^2 -
      pol_N1B_roots_1^2*r1B^2*r2A - 
      r1A*r1B^2*r2B + r1A*r1B*r2B^2 + 
      r1B^2*r2A*r2B - r1B*r2A*r2B^2 + D*r1A*r2A*r2B + D^2*r1A^2 + D^2*r2A^2 - 
      D^2*r1B^2 - D^2*r2B^2 - 2*D^2*r1A*r2A - D*r2A^2*r2B + D*r1A*r1B^2 - 
      D*r1A*r2B^2 + 2*D^2*r1B*r2B - D*r1B^2*r2A + D*r2A*r2B^2 + D*r1A*r1B*r2A
    N1B_z2_12 <- 0
    N1B_z3_12 <- (D*r1A^2 - D*r1A*r1B - 2*D*r1A*r2A + D*r1A*r2B + D*r1B*r2A + D*r2A^2 -
                    D*r2A*r2B + r1A*r1B*r2B - r1A*r2B^2 - r1B*r2A*r2B + r2A*r2B^2)
    
    N1B_possible <- NULL
    
    for(i in 1:length(N1B_z1_12)){
      N1B_i <- polyroot(c(N1B_z1_12[i],N1B_z2_12,N1B_z3_12))^2
      N1B_possible <- c(N1B_possible,N1B_i)
    }
    
    feasible_solutions_N1B <- which(round(Im(N1B_possible),10) == 0 & Re(N1B_possible) > 0)
    N1B_feasible <- Re(N1B_possible[feasible_solutions_N1B]) %>% unique()
    N1B_feasible
    
    eq_abundances <- expand.grid(N1B=N1B_feasible, N2B=N2B_feasible)
    
    eq_abundances$N2A <- eq_abundances$N2B - eq_abundances$N2B*(r2B - eq_abundances$N2B - eq_abundances$N1B)/D
    eq_abundances$N1A <- eq_abundances$N1B - eq_abundances$N1B*(r1B - eq_abundances$N2B - eq_abundances$N1B)/D
    
    
    eq_abundances_final <- eq_abundances %>%
      mutate(cond_1 = N1A*(r1A-N1A-N2A)+D*(N1B-N1A),
             cond_2 = N1B*(r1B-N1B-N2B)+D*(N1A-N1B),
             cond_3 = N2A*(r2A-N1A-N2A)+D*(N2B-N2A),
             cond_4 = N2B*(r2B-N1B-N2B)+D*(N2A-N2B)
      )
    
    eq_abundances_LV <- eq_abundances_final  %>%
      filter(abs(cond_1)<1e-10,abs(cond_2)<1e-10,abs(cond_2)<1e-10,abs(cond_3)<1e-10)
    
    return(nrow(eq_abundances_LV))
    
  }else{
    return(NA)
  }
  
  
  
}
