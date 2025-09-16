
eval_global_stability <- function(r1A,r1B,r2A,r2B,
                                  A11,A12,A21,A22,
                                  N1A,N1B,N2A,N2B,D){
  
  dN1Adt <- expression(r1A*N1A + A11*N1A*N1A + A12*N1A*N2A + D*(N1B - N1A))
  dN2Adt <- expression(r2A*N2A + A21*N1A*N2A + A22*N2A*N2A + D*(N2B - N2A))
  dN1Bdt <- expression(r1B*N1B + A11*N1B*N1B + A12*N1B*N2B - D*(N1B - N1A))
  dN2Bdt <- expression(r2B*N2B + A21*N1B*N2B + A22*N2B*N2B - D*(N2B - N2A))
  
  ddN1AdN1A <-D(dN1Adt, "N1A")
  ddN1AdN2A <-D(dN1Adt, "N2A")
  ddN1AdN1B <-D(dN1Adt, "N1B")
  ddN1AdN2B <-D(dN1Adt, "N2B")
  
  ddN2AdN1A <-D(dN2Adt, "N1A")
  ddN2AdN2A <-D(dN2Adt, "N2A")
  ddN2AdN1B <-D(dN2Adt, "N1B")
  ddN2AdN2B <-D(dN2Adt, "N2B")
  
  ddN1BdN1A <-D(dN1Bdt, "N1A")
  ddN1BdN2A <-D(dN1Bdt, "N2A")
  ddN1BdN1B <-D(dN1Bdt, "N1B")
  ddN1BdN2B <-D(dN1Bdt, "N2B")
  
  ddN2BdN1A <-D(dN2Bdt, "N1A")
  ddN2BdN2A <-D(dN2Bdt, "N2A")
  ddN2BdN1B <-D(dN2Bdt, "N1B")
  ddN2BdN2B <-D(dN2Bdt, "N2B")
  
  J <- expression( matrix(c(eval(ddN1AdN1A), eval(ddN1AdN2A),eval(ddN1AdN1B), eval(ddN1AdN2B),
                            eval(ddN2AdN1A), eval(ddN2AdN2A),eval(ddN2AdN1B), eval(ddN2AdN2B),
                            eval(ddN1AdN1B), eval(ddN1BdN2A),eval(ddN1BdN1B), eval(ddN1BdN2B),
                            eval(ddN2BdN1A), eval(ddN2BdN2A),eval(ddN2BdN1B), eval(ddN2BdN2B)),
                          nrow=4, byrow=TRUE) )
  
  J_example <- eval(J)
  
  eigStable <- eigen(J_example)
  eigStable[["values"]]
  
  if(all(Re(eigStable[["values"]])<0)){return(TRUE)}else{FALSE}

}
