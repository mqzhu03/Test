# 1
library(ade4)
library(tidyverse) 
data(doubs)
doubs
class(doubs)

# 2
env <- doubs$env
site <- row.names(env)
frame <- data.frame(env,site)
frame <- as_tibble(frame)
env_tb <- frame
class(env_tb)
  
#3
#3.1
dfs <- env_tb$dfs
dfs <- subset(dfs,dfs > 1000)
#3.2
env_final <- env_tb %>% select('site', 'dfs', 'slo', 'flo', 'pH', 'nit', 'oxy')
#3.3
names(env_final) <- c('site','distsour', 'slope','flowrate','pH','nitrogen','oxygen')
#3.4
env_final <- env_final[order(env_final$slope),]
env_final <- env_final[order(env_final$pH,decreasing = T),]
