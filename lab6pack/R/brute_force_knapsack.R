brute_force_knapsack <- function(x,W){
  stopifnot(is.data.frame(x),
            is.numeric(W),
            W>0,
            is.numeric(x$w),
            is.numeric(x$v),
            x$v>=0,
            x$w>=0)
  
  
  id=c(1:nrow(x))
  elements<-c()
  value<-0
  for (a in 1:nrow(x)){
    y=as.matrix(combn(id,a))
    for (i in 1:ncol(y)){
      tv<-0
      tw<-0
      for (j in 1:nrow(y)){
        tw <- tw + x$w[y[j,i]]
        tv <- tv + x$v[y[j,i]]
      }
      if(tw<=W && tv>=value){
        value=max(value,tv)
        elements= y[,i]
      }
    }
  }
  output<-list(
    value = round(value),
    elements = elements)
  return(output)
}