summarised_grand <- res %>% 
  group_by(M) %>% 
  summarise(mean_best_iter = mean(best_iter),
            mean_best_t = mean(best_t),
            mean_best_jl = mean(min_jl))

summarised_log <- res %>% filter(M=='log') %>% 
  group_by(T,Iter) %>% 
  summarise(mean_best_iter = mean(best_iter),
            mean_best_t = mean(best_t),
            mean_best_jl = mean(min_jl))
summarised_exp <- res %>% filter(M=='exp') %>% 
  group_by(T,Iter) %>% 
  summarise(mean_best_iter = mean(best_iter),
            mean_best_t = mean(best_t),
            mean_best_jl = mean(min_jl))
summarised_rec <- res %>% filter(M=='rec') %>% 
  group_by(T,Iter) %>% 
  summarise(mean_best_iter = mean(best_iter),
            mean_best_t = mean(best_t),
            mean_best_jl = mean(min_jl))
ggplot()+
  geom_line(data=res %>% mutate(idx=rep(1:90,3)), aes(x=idx,y=min_jl ,color =M))

ggplot()+
  geom_line(data=res %>% mutate(idx=rep(1:10,27)),
            aes(x=idx,y=min_jl ,color =M))+
  facet_grid(T~Iter)+
  xlab('Validation Index')+
  ylab('Minimum distance')
ggplot()+
  geom_line(data=log,aes(x=c,y=best_iter))+
  xlab('Reduction Rate')+
  ylab('Number of iteration to optimal')+
  ggtitle('Number of iteration until optimum')+
  theme(plot.title = element_text(hjust = 0, size = 16))
###
n_grid <- 1:100
rexp <- function(Tmp,n,r) Tmp*r^n
rlog <- function(Tmp,n) Tmp/log(n+1)
rrec <- function(Tmp,n) Tmp/(n+1)
Tmp <- 500
dexp <- rexp(Tmp,n_grid,0.9)
dlog <- rlog(Tmp,n_grid)
drec <- rrec(Tmp,n_grid)
temp_red <- tibble(n = n_grid,Exponential=dexp,Log=dlog,Inverse=drec)
temp_red <- temp_red %>% pivot_longer(cols = 2:4,names_to = 'Method',values_to = 'f(x)')
ggplot()+
  geom_line(data=temp_red,aes(x=n,y=`f(x)`,color=Method))+
  ggtitle('Temperature reduce functions')

