
eval_global_stability_PERFECT_1sp <- function(r1A,r1B,
                                  A11,A22,
                                  N1A,N1B,D){
  
  dN1Adt <- expression(r1A*N1A + A11*N1A*N1A + D*(N1B - N1A))
  dN1Bdt <- expression(r1B*N1B + A11*N1B*N1B - D*(N1B - N1A))
  
  ddN1AdN1A <-D(dN1Adt, "N1A")
  ddN1AdN1B <-D(dN1Adt, "N1B")
  
  ddN1BdN1A <-D(dN1Bdt, "N1A")
  ddN1BdN1B <-D(dN1Bdt, "N1B")
  
  
  J <- expression( matrix(c(eval(ddN1AdN1A), eval(ddN1AdN1B),
                            eval(ddN1AdN1B), eval(ddN1BdN1B)),
                          nrow=2, byrow=TRUE) )
  
  J_example <- eval(J)
  
  eigStable <- eigen(J_example)
  eigStable[["values"]]
  
  if(all(Re(eigStable[["values"]])<0)){return(TRUE)}else{FALSE}

}
