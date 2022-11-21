genseq <- function(sq) {  ##Generating a random route
  n<-length(sq);
  if(runif(1,0,1)<0.25)
  {
    index <- sample(2:(n-1), size=2, replace=FALSE);
    tmp <- sq[index[1]];
    sq[index[1]] <- sq[index[2]];
    sq[index[2]] <- tmp;
  }else
  {
    index <- sort(sample(2:(n-1), size=3, replace=FALSE));
    old_sq<-sq;
    sq[index[1]:index[3]]<-c(old_sq[index[2]:index[3]],old_sq[index[1]:(index[2]-1)]);
  }
  return(sq)
}