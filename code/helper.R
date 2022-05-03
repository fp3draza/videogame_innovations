power_law <- function(x,lambda,beta){
  return(lambda*(x**beta))
}

straight_line <- function(x,p,m){
  return(x*m +p)
}