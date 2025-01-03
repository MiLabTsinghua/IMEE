################################################################################
### load package
library(readr)
library(readxl)
library(dplyr)
### load fun
source('./script/00_helperfun.R')

################################################################################
## load data
data = readRDS('./data/neurondata.RDS')
data_cord = data$cord
data_t = data$t
data_dt = diff(data_t)
data_meta = data$meta
data_meta_group = data$meta_group

data_dis = list()
data_vel = list()
for (t_index in 1:length(data_dt)) {
  s_index_m = t_index
  s_index_p = t_index + 1
  print(paste('time index, backward index, forward index:', t_index, s_index_m, s_index_p))
  data_dis[[t_index]] = data_cord[[s_index_p]] - data_cord[[s_index_m]]
  data_vel[[t_index]] = data_dis[[t_index]]/data_dt[t_index]
}

### load grid
x_grid <- read_csv("./data/x_grid.csv")
y_grid <- read_csv("./data/y_grid.csv")
bf_grid <- read_csv("./data/bf_grid.csv")

################################################################################
### generate (u,v)-grid
uv_grid = list()
alpha = 0.10
sigma2_vel = 1e3

for (s_index in 1:6) {
  t_index = ifelse(s_index %% 2 == 1, (s_index+1)/2, s_index/2)
  print(paste('s_index, t_index:', s_index, t_index))
  
  if(s_index %% 2 == 1){
    temp_id = which(data_meta == 'mz')
  } else {
    temp_id = which(data_meta == 'svz')
  }
  hhd_cord = data_cord[[t_index]][temp_id,1:2] %>% data.frame()
  hhd_vel  = data_vel[[t_index]][temp_id,1:2] %>% data.frame()
  
  # using knn based on metrics
  hhd_cord_dist = hhd_cord %>% dist() %>% as.vector()
  radius = quantile(hhd_cord_dist, alpha, na.rm = T) %>% as.numeric()
  uv_grid[[s_index]] = vfsmooth(x_grid, y_grid, bf_grid, hhd_cord, hhd_vel, radius, sigma2_vel)
  
  print(s_index)
  write.csv(u_grid_2, paste0('./output/u_grid_0',as.character(s_index),'.csv'), row.names = F)
  write.csv(v_grid_2, paste0('./output/v_grid_0',as.character(s_index),'.csv'), row.names = F)
}


