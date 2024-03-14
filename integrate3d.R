integrate3d <- function(f, over, n)
{

  x_ax <- runif(n, min = over$x[1], max = over$x[2])
  y_ax <- runif(n, min = over$y[1], max = over$y[2])
  
  f_val_vector <- f(x_ax, y_ax)
  
  
  z_ax <- runif(n, min = 0, max = max(f_val_vector))
  
  dx <- over$x[2] - over$x[1]
  dy <- over$y[2] - over$y[1]
  dz <- max(f_val_vector)
  
  under_f <- 0
    
  for (i in 1:n)
  {
    f_val <- f(x_ax[i], y_ax[i])
    
    if (z_ax[i] <= f_val)
    {
      under_f <- under_f + 1
    }
  }
  
  return(dx * dy * dz * (under_f / n))
}


##########################################
############## PRZYKLAD 1#################
##########################################
### (low n)
integrate3d(
  f = function(x, y) {cos(x) * y},
  over = list(x = c(0, pi / 2), y = c(0, 1)),
  n = 10^2)

### (high n)
integrate3d(
  f = function(x, y) {cos(x) * y},
  over = list(x = c(0, pi / 2), y = c(0, 1)),
  n = 10^5)


##########################################
############## PRZYKLAD 2#################
##########################################
### (low n)
integrate3d(
  f = function(x, y) { (cos(x) + 2) * (sin(y) + 1)},
  over = list(x = c(0, pi), y = c(0, pi)),
  n = 10^2)

### (high n)
integrate3d(
  f = function(x, y) { (cos(x) + 2) * (sin(y) + 1)},
  over = list(x = c(0, pi), y = c(0, pi)),
  n = 10^5)


##########################################
############## PRZYKLAD 3#################
##########################################
integrate3d(
  f = function(x, y) {6 * x^3 * y^2},
  over = list(x = c(1, 2), y = c(0, 1)),
  n = 10^5
)

#wynik 7.5
##########################################
############## PRZYKLAD 4#################
##########################################
integrate3d(
  f = function(x, y) {x + sin(y) + 1},
  over = list(x = c(0, 2), y = c(-pi, pi)),
  n = 10^5
)

#wynik pi * 8


















