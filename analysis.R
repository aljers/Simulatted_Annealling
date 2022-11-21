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
        x<-sann(m_dis=m_dis, sq=sq, 
                m_iter=1000, t_iter=iter, 
                t=t, method=m, d_rate=0.97);
        res$M[k]<-m;
        res$T[k]<-t;
        res$Iter[k]<-iter;
        res$max_iter[k]<-nrow(x$log)-1;
        res$min_jl[k]<-x$jl;
        best_iter<-min(which(x$log$dis==x$jl));
        res$best_iter[k]<-best_iter;
        res$best_t[k]<-x$log$t[best_iter];
        k<-k+1;
      }
    }
  }
}