
number_feasible_and_stable_solutions_2sp_toy_DOM_PERFECT_MASTER <-  function(D,r1A,r1B,r2A,r2B){
  
  if(r1A == 0 & r1B==0){ #OK
    return(c(0,0))
  }else if(r2A == 0 & r2B==0){ #OK
    return(c(0,0))
  # }else if(r1A == r1B){
  #   return(number_feasible_solutions_2sp_toy_DOM_PEREFCT_equal_rA(D,r1A,r1B,r2A,r2B))
  # }else if(r2A == r2B){
  #   return(number_feasible_solutions_2sp_toy_DOM_PEREFCT_equal_rA(D,r2A,r2B,r1A,r1B))
  }else{
    return(number_feasible_solutions_2sp_toy_DOM_PERFECT_all_values(D,r1A,r1B,r2A,r2B))
  }
}
