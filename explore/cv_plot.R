library(ggplot2)

dist_res <- function(matrix){

   residual <- matrix$liq_total - matrix$pred_liq_total
   distribucion <- ggplot(matrix, aes(x=residual)) + 
                  geom_density() + geom_vline(aes(xintercept=mean(residual, na.rm=T)),   # Ignore NA values for mean
                  color="red", linetype="dashed", size=1) + theme_bw()
   
   return(distribucion)
}

dist_res2 <- function(matrix){
  
  residual <- matrix$liq_total - matrix$pred_liq_total
  residual2 <- residual^2
  distribucion2 <- ggplot(matrix, aes(x=residual2)) + 
    geom_density() + geom_vline(aes(xintercept=mean(residual2, na.rm=T)),   # Ignore NA values for mean
                                color="red", linetype="dashed", size=1) + theme_bw()
  
  return(distribucion2)
}

lln_corr <- function(matrix){
  
  corr <- NULL
  
  for(i in 1:1000){
  j <- 1+(1000)*(i-1)
  k <- i*1000
  a <- matrix$liq_total[j:k]
  b <- matrix$pred_liq_total[j:k]
  corr[i] <- cor(a , b, use="pairwise.complete.obs")
  }
  return(corr)
}
  
