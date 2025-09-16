
number_feasible_and_stable_solutions_2sp_toy_DOM_HALF_MASTER <-  function(D,r1A,r1B,r2A,r2B){
  
  if(r1A == 0 & r1B==0 & r2A == r2B){ #OK
    return(c(0,0))
  }else if(r2A == 0 & r2B==0 & r1A == r1B){ #OK
    return(c(0,0))
  }else if(r1A == r2A){ #OK
    return(c(0,0))
  }else if(r1B == r2B){ #OK
    return(c(0,0))
  }else if(r1A == r1B & r1A != 0 & r2A == r2B & r2A != 0){
    return(number_feasible_solutions_2sp_toy_DOM_ZERO_2_values(D,r1A,r1B,r2A,r2B))
  }else{
    return(number_feasible_solutions_2sp_toy_DOM_ZERO_4_values(D,r1A,r1B,r2A,r2B))
  }
}
