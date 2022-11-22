dis<-function(tsp,label) 
{
  n<-length(label);
  m_dis<-matrix(0,n,n);
  colnames(m_dis)<-label;
  rownames(m_dis)<-label;
  k<-1;
  for(i in 1:(n-1))
  {
    m_dis[i,(i+1):n]<-tsp[k:(k+n-i-1)];
    k<-k+n-i;
  }
  
  
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      if(j<i)
      {
        m_dis[i,j]<-m_dis[j,i]
      }
    }
  }
  return(m_dis);
}

distance <- function(m_distance, sq){
  res <- 0
  for (i in 1:(length(sq)-1)){
    res <- res + m_distance[sq[i],sq[i+1]]
  }
  return(res)
}
