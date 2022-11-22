#Starting the initializing
source('setup.R')

T<-c(500,1000,5000) ##initial temperature
Method<-c("exp","log","rec") ##cooling method
Iter<-c(10,30,50) ##inner iteration number
count<-10 ##number of validation of each combination of parameters
res<-data.frame(idx=1:(27*count),M="",T=0,Iter=0,max_iter=0,min_jl=0,best_iter=0,best_t=0);
res$M<-as.character(res$M);
k<-1;##iteration times
for(m in Method)
{
  for(t in T)
  {
    for(iter in Iter)
    {
      for(i in 1:count) 
      {
        x<-sann(m_dis=m_dis, sq=NULL, 
                m_iter=1000, t_iter=iter, 
                t=t, method=m, d_rate=0.97);
        res$M[k]<-m;
        res$T[k]<-t;
        res$Iter[k]<-iter;
        res$max_iter[k]<-nrow(x$log)-1;
        res$min_jl[k]<-min(x$log$dis);
        best_iter<-min(which(x$log$dis==min(x$log$dis)));
        res$best_iter[k]<-best_iter;
        res$best_t[k]<-x$log$t[best_iter];
        k<-k+1;
      }
    }
  }
}
#optimal parameters
iter_opt <- res$Iter[which.min(res$min_jl)]
t_opt <- res$T[which.min(res$min_jl)]



C_grid <- seq(0.05,0.95,0.05)
log <- data.frame(c=C_grid)
k <- 1
for (c in C_grid){
  x<-sann(m_dis=m_dis, sq=NULL, 
          m_iter=1000, t_iter=30, 
          t=5000, method='exp', d_rate=c);
  log$max_iter <- nrow(x$log)-1
  log$min_jl[k]<-min(x$log$dis)
  best_iter<-min(which(x$log$dis==min(x$log$dis)));
  log$best_iter[k]<-best_iter;
  log$best_t[k]<-x$log$t[best_iter];
  
  k <- k+1
}
