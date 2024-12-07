################################################################################
### load package
library(readr)
library(readxl)
library(dplyr)
library(pheatmap)

################################################################################
### generate speed
uvw_grid = list()
for (s_index in 1:length(uv_grid)) {
  u_grid_2 = uv_grid[[s_index]]$u
  v_grid_2 = uv_grid[[s_index]]$v
  w_grid_2 = sqrt(u_grid_2^2 + v_grid_2^2)
  uvw_grid[[s_index]] = w_grid_2
}

################################################################################
### heatmap
vel_cor = matrix(0, nrow = 6, ncol = 6)
for (i in 1:6) {
  for (j in 1:6) {
    vel_cor[i,j] = (uvw_grid[[i]]-uvw_grid[[j]])^2 %>% surf_integrate() %>% sqrt()
  }
}
rownames(vel_cor) = data_meta_group$comp
colnames(vel_cor) = data_meta_group$comp
vel_cor

pheatmap(vel_cor/max(vel_cor),
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean")


