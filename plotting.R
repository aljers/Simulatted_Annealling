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
  geom_count(data=res, aes(x=idx,y=min_jl ,color =M))

ggplot()+
  geom_line(data=res,
            aes(x=idx,y=min_jl ,color =M))+
  facet_grid(T~Iter)+
  xlab('Validation Index')+
  ylab('Minimum distance')
ggplot()+
  geom_line(data=log,aes(x=c,y=best_iter))+
  xlab('Reduction Rate')+
  ylab('Number of iteration to optimal')
