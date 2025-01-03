
### basics
euclidean2 = function(v){
  return(sum(v^2))
}

euclidean = function(v){
  return(sqrt(sum(v^2)))
}

### extract knn id
knn_radius = function(v, df, radius = NULL){
  n = nrow(df)
  delta = outer(rep(1,n), v, '*') - df
  d = apply(delta, 1, euclidean)
  
  id = which(d <= radius)
  return(as.numeric(id))
}

### generate normalized convex coefs
rbf_prop = function(v, df, sigma2 = 1){
  n = nrow(df)
  delta = outer(rep(1, n), v, '*') - df
  d2 = apply(delta, 1, euclidean2)
  prop = exp(-1/(2*sigma2)*d2)
  return(prop/sum(prop))
}

### generate smoothed vector field
vfsmooth = function(x_grid, y_grid, bf_grid, cord, vel, r, sigma2){#
  x_grid = x_grid[1,]
  y_grid = y_grid[,1]
  l_x = length(x_grid)
  l_y = length(y_grid)
  
  u_grid = matrix(0, nrow = l_y, ncol = l_x)
  v_grid = matrix(0, nrow = l_y, ncol = l_x)
  
  for (i in 1:l_x) {
    for (j in 1:l_y){
      cord_ij = c(x_grid[i],y_grid[j])
      knn_id = knn_radius(cord_ij, cord, r)
      if (length(knn_id) >= 2){
        sub_cord = cord[knn_id,]
        vec_coef = rbf_prop(cord_ij, sub_cord, sigma2)
        u_grid[j,i] = as.numeric(vec_coef %*% vel[knn_id,1])
        v_grid[j,i] = as.numeric(vec_coef %*% vel[knn_id,2])
      } else if (length(knn_id == 1)) {
        u_grid[j,i] = vel[knn_id,1]
        v_grid[j,i] = vel[knn_id,2]
      } else {
        u_grid[j,i] = 0
        v_grid[j,i] = 0
      }
    }
  }
  
  u_grid_2 = u_grid
  v_grid_2 = v_grid
  for (i in 1:l_x) {
    u_grid_2[which(bf_grid[,i] >= 0),i] = 0
    v_grid_2[which(bf_grid[,i] >= 0),i] = 0
  }
  
  return(list(u = u_grid_2, v = v_grid_2))
  
}

################################################################################
### L2 norm
surf_integrate = function(z_grid){
  lx = ncol(z_grid)
  ly = nrow(z_grid)
  z_int = z_grid[1:(ly-1),1:(lx-1)]
  for (i in 1:(lx-1)) {
    for (j in 1:(ly-1)) {
      z_value = z_grid[j:(j+1),(i:(i+1))] %>% c()
      z_int[j,i] = mean(z_value)
    }
  }
  return(sum(z_int))
}

################################################################################









