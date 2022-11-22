set.seed(9864)
tsp<-scan("tsp.txt",sep=",") 
##cleaning
tsp<-na.omit(tsp)
##Labeling each city
label<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q",
         "R","S","T","U","V","W","X","Y","Z","AA","AB","AC","AD","AE") 


m_dis<-dis(tsp,label)

##
sann<-function(m_dis,sq=NULL,m_iter=100,t_iter=50,t=100000,method="exp",d_rate=0.8)
{
 

  ##initial route
  if(is.null(sq)){
      sq <- c(1,2:NROW(m_dis),1)
      }
  ##initial distance
  jl<-distance(m_dis,sq);
  ##optimization
  log<-data.frame(iter=0:(m_iter*t_iter),dis=0,t=0,sq="");
  log$sq<-as.character(log$sq);
  log$dis[1]<-jl;
  log$t[1]<-t;
  log$sq[1]<-str_c(sq,collapse=",");
  
  k<-1;##iteration index
  t0<-t;
  
  ##Loop for temperature
  for(i in 1:m_iter){
    ##Loop for 
    for(j in 1:t_iter){
      ##Generate new route by randomly adjusting two of nodes
      tmp_sq<-genseq(sq);
      ##evaluate total distance of current route
      tmp_jl<-distance(m_dis,tmp_sq);
      if(tmp_jl<jl){
        jl<-tmp_jl;
        sq<-tmp_sq;
        }
      else{
        if(exp((jl-tmp_jl)/5*t)>runif(1,0,1)){
          jl<-tmp_jl;
          sq<-tmp_sq;
          }
        }
      log$dis[k+1]<-jl;
      log$t[k+1]<-t;
      log$sq[k+1]<-str_c(sq,collapse=",");
      k<-k+1;	
    }
    ##Temperature decrease
    if(method=="exp"){
      t<-t*d_rate;
      }
    else if(method=="log")
    {
      t<-t0/log(i+1);
      }
    else if(method=="rec"){
      t<-t0/(i+1);
      }
    else{
      t<-t*d_rate;
      }
  }
  return(list(sq=sq,jl=jl,log=log));
}