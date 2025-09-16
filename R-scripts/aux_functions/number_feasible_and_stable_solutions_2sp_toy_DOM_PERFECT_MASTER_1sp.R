
number_feasible_and_stable_solutions_2sp_toy_DOM_PERFECT_MASTER_1sp <-  function(D,r1A,r1B){
  
  if(r1A == 0 & r1B==0){ #OK
    return(c(0,0))
  }else{
    return(number_feasible_solutions_2sp_toy_DOM_PERFECT_all_values_1sp(D,r1A,r1B))
  }
}
