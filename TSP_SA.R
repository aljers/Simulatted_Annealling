set.seed(9864)
tsp<-scan("tsp.txt",sep=",") 
tsp<-na.omit(tsp) ##cleaning
label<-c("北京","上海","天津","石家庄","太原","呼和浩特","沈阳","长春","哈尔滨",
         "济南","南京","合肥","杭州","南昌","福州","台北","郑州","武汉","长沙",
         "广州","海口","南宁","西安","银川","兰州","西宁","乌鲁木齐","成都",
         "贵阳","昆明","拉萨") ##Labeling each city

##Distance matrix
source('distance.R')
##Generating randomised route
source('route.R')
m_dis<-dis(tsp,label)

##
sann<-function(m_dis,sq,m_iter=100,t_iter=50,t=100000,method="exp",d_rate=0.97)
{
  require(stringr);

  ##initial route
  if(is.null(sq))
  {
    sq <- c(1,2:NROW(m_dis),1);
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
  for(i in 1:m_iter)
  {
    ##Loop for 
    for(j in 1:t_iter)
    {
      ##Generate new route by randomly adjusting two of nodes
      tmp_sq<-genseq(sq);
      ##evaluate total distance of current route
      tmp_jl<-distance(m_dis,tmp_sq);
      if(tmp_jl<jl)
      {
        jl<-tmp_jl;
        sq<-tmp_sq;
      }else
      {
        if(exp((jl-tmp_jl)/t)>runif(1,0,1))
        {
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
    if(method=="exp")
    {
      t<-t*d_rate;
    }else if(method=="log")
    {
      t<-t0/log(i+1);
    }else if(method=="rec")
    {
      t<-t0/(i+1);
    }else
    {
      t<-t*d_rate;
    }
    
  }
  
  return(list(sq=sq,jl=jl,log=log));
}